use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn rust_capitalize(s: *mut c_char) {
    unsafe {
        let mut p = s as *mut u8;
        while *p!=0 {
            let ch = char::from(*p);
            if ch.is_ascii() {
                let upper = ch.to_ascii_uppercase();
                *p = upper as u8;
            }
            p = p.offset(1);
        }
    }
}