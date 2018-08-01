fn main() {
    let raw_p: *const u32 = &10;
    unsafe {
        assert!(*raw_p == 10);
    }
    let u:&[u8] = &[49,50,51];
    unsafe {
        assert!(u == std::mem::transmute::<&str, &[u8]>("123"));
    }
}