extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use syn::{parse_macro_input, FnArg, ImplItemMethod, ItemImpl, Pat, Signature};

#[proc_macro_attribute]
pub fn objc_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr_string = attr.to_string();
    let attributes = parse_export_objc_attributes(&attr_string);

    // println!("input: {}", item.to_string());
    let input = parse_macro_input!(item as ItemImpl);
    let input_type = input.self_ty.clone();

    let super_class_name = Ident::new(&attributes.super_class_name, Span::call_site());
    let protocol_name = attributes
        .protocol
        .map(|val| Ident::new(&val, Span::call_site()));

    let protocol_block = if let Some(protocol_name) = protocol_name {
        quote! {
            {
            let p = Protocol::get(stringify!(#protocol_name)).unwrap();
            decl.add_protocol(p);
            }
        }
    } else {
        quote! {
            {}
        }
    };

    let all_methods = input
        .items
        .iter()
        .filter_map(|val| match val {
            syn::ImplItem::Method(val) => {
                if val.attrs.is_empty() {
                    return None;
                }

                let path = val.attrs[0].path.clone();
                if path.is_ident("selector_init") {
                    let add_method = quote! {
                        decl.add_method(
                            objc::sel!(init),
                            #input_type::generated_init as extern "C" fn(&Object, _) -> id,
                        );
                    };
                    Some(add_method)
                } else if path.is_ident("selector_impl") {
                    let objc_delegate = &val.attrs[0].tokens.to_string();
                    // let objc_delegate = quote!(#objc_delegate);
                    let objc_delegate_name = get_objc_selector_full(&*objc_delegate);
                    let objc_selector_token: proc_macro2::TokenStream =
                        objc_delegate_name.parse().unwrap();
                    let sel_func_type = generate_sel_function_type(val);
                    let sel_func_name = generate_sel_function_name(val);

                    let add_method = quote! {
                        decl.add_method(
                            objc::sel!(#objc_selector_token),
                            #input_type::#sel_func_name as #sel_func_type,
                        );
                    };
                    Some(add_method)
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let dealloc_method = quote! {
        decl.add_method(
            objc::sel!(dealloc),
            #input_type::generated_dealloc as extern "C" fn(&objc::runtime::Object, _),
        );
    };

    let input_type_string = quote! {
        #input_type
    }
    .to_string();
    let objc_class_ident = format_ident!("RUST_{}", input_type_string);
    let r = quote! {
        impl #input_type {
            pub fn objc_class_name() -> &'static str {
                stringify!(#objc_class_ident)
            }

            pub fn register_rust_class() -> *const objc::runtime::Class {
                use objc::{
                    class,
                    declare::ClassDecl,
                    msg_send,
                    runtime::{Class, Object, Protocol, Sel},
                    sel, sel_impl,
                };

                static mut CLASS_POINTER: *const objc::runtime::Class = 0 as *const objc::runtime::Class;
                static INIT: std::sync::Once = std::sync::Once::new();

                INIT.call_once(|| unsafe {
                    let class_name = Self::objc_class_name();

                    let superclass = objc::class!(#super_class_name);
                    let mut decl = objc::declare::ClassDecl::new(class_name, superclass).unwrap();

                    decl.add_ivar::<usize>("RUST_OBJ_PTR");

                    #protocol_block

                    #(#all_methods)*

                    #dealloc_method


                    // Launching Applications
                    CLASS_POINTER = decl.register();
                });

                unsafe { CLASS_POINTER }
            }

            pub fn init_objc_proxy_obj(self: std::sync::Arc<Self>) -> *mut objc::runtime::Object {
                use objc::{
                    class,
                    declare::ClassDecl,
                    msg_send,
                    runtime::{Class, Object, Protocol, Sel},
                    sel, sel_impl,
                };

                let class = #input_type::register_rust_class();
                let objc_object = unsafe {
                    let ret: *mut objc::runtime::Object = msg_send![class, new];
                    ret
                };
                let raw_ptr = std::sync::Arc::into_raw(self);
                let raw_ptr_value = raw_ptr as usize;

                unsafe {
                    (&mut *objc_object).set_ivar("RUST_OBJ_PTR", raw_ptr_value);
                }

                objc_object
            }

            extern "C" fn generated_dealloc(this: &objc::runtime::Object, _: objc::runtime::Sel) {
                let arc = unsafe {
                    let raw_ptr_value: usize = *this.get_ivar("RUST_OBJ_PTR");
                    let raw_ptr = raw_ptr_value as *const Self;
                    std::sync::Arc::from_raw(raw_ptr)
                };
                drop(arc);
            }
        }

        #input
    };

    r.into()
}

#[proc_macro_attribute]
pub fn selector_impl(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ImplItemMethod);

    let method_name = input.sig.ident.clone();

    let new_sig = generate_sel_function(&input);
    let args = generate_sel_function_args(&input);
    let fn_body = quote! {
        {
            let arc = unsafe {
                let raw_ptr_value: usize = *this.get_ivar("RUST_OBJ_PTR");
                let raw_ptr = raw_ptr_value as *const Self;
                std::sync::Arc::from_raw(raw_ptr)
            };

            let ret = arc.#method_name(this, #(#args,)*);
            std::sync::Arc::into_raw(arc);
            ret
        }
    };

    let generated_func = quote! {
        #new_sig
            #fn_body
    };

    let ret = quote! {
        #input

        #generated_func
    };
    ret.into()
}

#[proc_macro_attribute]
pub fn selector_init(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ImplItemMethod);

    let method_name = input.sig.ident.clone();

    let ret = quote! {
        #input

        extern "C" fn generated_init(this: &Object, _: objc::runtime::Sel) -> id {
            use objc::{
                class,
                declare::ClassDecl,
                msg_send,
                runtime::{Class, Object, Protocol, Sel},
                sel, sel_impl,
            };

            Self::register_rust_class();
            let arc = Self::#method_name();

            let objc_object = unsafe {
                // TODO: don't hard code UIResponder
                let ret: *mut objc::runtime::Object = msg_send![super(this, class!(UIResponder)), init];
                ret
            };
            let raw_ptr = std::sync::Arc::into_raw(arc);
            let raw_ptr_value = raw_ptr as usize;

            unsafe {
                (&mut *objc_object).set_ivar("RUST_OBJ_PTR", raw_ptr_value);
            }

            objc_object
        }
    };
    ret.into()
}

#[proc_macro_attribute]
pub fn selector_export(attr: TokenStream, item: TokenStream) -> TokenStream {
    // eprintln!("attr {}", attr);
    // let attr_string = attr.to_string();

    let input = parse_macro_input!(item as ImplItemMethod);

    let attr_string = attr.to_string();
    let attributes = parse_trait_selector_attributes(&attr_string);
    // eprintln!("attr {:?}", attributes);

    let objc_selector = ObjcSelector::from(attributes.selector.as_str());
    // eprintln!("objc_selector {:?}", objc_selector);

    let message = if objc_selector.n_args == 0 {
        let arg_name = Ident::new(&objc_selector.parts[0], Span::call_site());
        quote!(#arg_name)
    } else {
        let arg_idents = function_input_idents(input.sig.clone(), attributes.class_name.is_some());
        let pair_iter = objc_selector
            .parts
            .into_iter()
            .zip(arg_idents.into_iter())
            .map(|(arg, arg_ident)| {
                let arg_name = Ident::new(&arg, Span::call_site());
                let token = quote! {
                    #arg_name: #arg_ident
                };
                token
            });
        quote!(
            #(#pair_iter )*
        )
    };

    // eprintln!("message {}", message);

    let vis = input.vis.clone();
    let input_sig = input.sig.clone();
    let ret_type = input_sig.output.clone();
    let ret_type = match ret_type {
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, type_) => quote!(#type_),
    };

    let target_object = if let Some(class_name) = attributes.class_name {
        let class_name_ident = Ident::new(&class_name, Span::call_site());
        quote! {
            let target_object = objc::class!(#class_name_ident);
        }
    } else {
        quote! {
            let target_object = self.objc_object();
        }
    };

    // eprintln!("target_object {}", target_object);
    let fn_body = quote! {
        {
            use objc::{
                class,
                declare::ClassDecl,
                msg_send,
                runtime::{Class, Object, Protocol, Sel},
                sel, sel_impl,
            };
            unsafe {
                #target_object

                let ret: #ret_type = msg_send![target_object, #message];
                ret
            }
        }
    };

    let ret = quote! {
        #vis #input_sig
            #fn_body
    };
    ret.into()
}

fn get_objc_selector_full(token: &str) -> &str {
    token
        .trim_start_matches('(')
        .trim_start_matches('"')
        .trim_end_matches(')')
        .trim_end_matches('"')
}

#[derive(Debug)]
struct ObjcSelector {
    parts: Vec<String>,
    n_args: usize,
}

impl From<&str> for ObjcSelector {
    fn from(input: &str) -> Self {
        let part_slit = input.split(':');
        let (parts, has_empty) = part_slit.fold(
            (Vec::<String>::new(), false),
            |(mut acc, has_empty), val| {
                if val != "" {
                    acc.push(val.to_string());
                    (acc, has_empty)
                } else {
                    (acc, true)
                }
            },
        );

        let n_args = if has_empty {
            parts.len()
        } else {
            parts.len() - 1
        };

        ObjcSelector { parts, n_args }
    }
}

fn generate_sel_function_name(input: &ImplItemMethod) -> proc_macro2::Ident {
    let orginal_name = input.sig.ident.to_string();
    let new_name_str = format!("generated_{}", orginal_name);
    let new_name_ident = Ident::new(&new_name_str, Span::call_site());

    new_name_ident
}

fn generate_sel_function_args(input: &ImplItemMethod) -> Vec<Box<Pat>> {
    let args = {
        let mut ret = input.sig.inputs.clone().into_iter().collect::<Vec<_>>();
        ret.remove(0);
        ret.remove(0);
        ret.into_iter()
            .map(|val| match val {
                FnArg::Receiver(_) => todo!(),
                FnArg::Typed(val) => val.pat,
            })
            .collect()
    };

    args
}

fn generate_sel_function(input: &ImplItemMethod) -> proc_macro2::TokenStream {
    let new_name_ident = generate_sel_function_name(input);

    let vis = input.vis.clone();
    let args = {
        let mut ret = input.sig.inputs.clone().into_iter().collect::<Vec<_>>();
        ret.remove(0);
        ret.remove(0);
        ret
    };
    let return_type = input.sig.output.clone();

    quote! {
        #vis extern "C" fn #new_name_ident(this: &objc::runtime::Object, _sel: objc::runtime::Sel,  #(#args,)*) #return_type
    }
}

fn generate_sel_function_type(input: &ImplItemMethod) -> proc_macro2::TokenStream {
    let args = {
        let mut ret = input.sig.inputs.clone().into_iter().collect::<Vec<_>>();
        ret.remove(0);
        ret.remove(0);
        ret
    };
    let return_type = input.sig.output.clone();

    quote! {
        extern "C" fn(this: &objc::runtime::Object, _sel: objc::runtime::Sel,  #(#args,)*) #return_type
    }
}

fn function_input_idents(sig: Signature, is_type_method: bool) -> Vec<Box<Pat>> {
    let args = {
        let mut ret = sig.inputs.clone().into_iter().collect::<Vec<_>>();
        if !is_type_method {
            ret.remove(0);
        }

        ret
    };

    let arg_idents = args
        .into_iter()
        .map(|val| match val {
            FnArg::Receiver(_) => panic!("unexpected receiver type"),
            FnArg::Typed(pattype) => pattype.pat,
        })
        .collect::<Vec<_>>();

    arg_idents
}

#[derive(Debug)]
struct ExportObjCAttributes {
    super_class_name: String,
    protocol: Option<String>,
}

fn parse_export_objc_attributes(attributes: &str) -> ExportObjCAttributes {
    let mut vals = attributes.split(',');

    let super_class_name = vals.next().unwrap().trim().to_string();
    let protocol = vals.next().map(|val| val.trim().to_string());

    ExportObjCAttributes {
        super_class_name,
        protocol,
    }
}

#[derive(Debug)]
struct TraitSelectorAttributes {
    class_name: Option<String>,
    selector: String,
}

fn parse_trait_selector_attributes(attributes: &str) -> TraitSelectorAttributes {
    let vals = attributes.split(',').collect::<Vec<_>>();

    if vals.len() == 1 {
        let selector = vals[0];
        let selector = get_objc_selector_full(selector).to_string();
        return TraitSelectorAttributes {
            class_name: None,
            selector,
        };
    }

    let class_name = vals[0].trim().to_string();
    let selector = vals[1].trim();
    let selector = get_objc_selector_full(selector).to_string();

    TraitSelectorAttributes {
        class_name: Some(class_name),
        selector,
    }
}

// fn ident_to_string(ident: &Ident) -> String {
//     format!("{}", ident)
// }

// /// syn -> string https://github.com/dtolnay/syn/issues/294
// fn type_to_string(ty: &Type) -> String {
//     quote!(#ty).to_string().replace(" ", "")
// }

#[cfg(test)]
mod tests {
    use crate::ObjcSelector;

    #[test]
    fn it_works() {
        let selector = "test:";
        let parts = ObjcSelector::from(selector);
        dbg!(parts);
        assert_eq!(2 + 2, 4);
    }
}
