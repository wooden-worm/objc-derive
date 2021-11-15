`objc-derive` allows Rust to interop with Objective-C seamlessly.

### Calling Objective-C

Example

```Rust
impl NSString {
    #[selector_export("initWithBytes:length:encoding:")]
    pub fn init_with_bytes_length_encoding(&self, bytes: *const c_void, length: usize, encoding: u64) -> NSString;

    #[selector_export("lengthOfBytesUsingEncoding:")]
    pub fn length_of_bytes_using_encoding(&self, encoding: u64) -> usize;

    #[selector_export("UTF8String")]
    pub fn utf8_string(&self) -> *const u8;
}
```

### Use Rust to implement Objective-C selectors

```Rust
#[objc_impl(UIViewController)]
impl MainViewController {
    #[selector_impl("loadView")]
    fn load_view(&self, this: &Object) {
        // Rust code here, will be called by UIKit
    }
    
    #[selector_impl("viewDidLoad")]
    fn view_did_load(&self, this: &Object) {
        // Rust code here
    }
}
```
