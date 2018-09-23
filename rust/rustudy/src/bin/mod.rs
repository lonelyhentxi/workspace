// my1
// struct_visibility
mod my {
    fn private_function() {
        println!("called `my1::private_function()`");
    }

    pub fn function() {
        println!("called `my1::function()`");
    }

    pub fn indirect_access() {
        print!("called `my1::indirect_access()`,than\n> ");
        private_function();
    }

    pub mod nested {
        pub fn function() {
            println!("called `my1::nested::function()`");
        }

        #[allow(dead_code)]
        fn private_function() {
            println!("called `my1::nested::private_function()`");
        }
    }

    mod private_nested {
        #[allow(dead_code)]
        pub fn function() {
            println!("called `my1::private_nested::function()`");
        }
    }

    pub struct WhiteBox<T> {
        pub contents: T,
    }

    #[allow(dead_code)]
    pub struct BlackBox<T> {
        contents: T,
    }

    impl<T> BlackBox<T> {
        pub fn new(contents:T) -> BlackBox<T> {
            BlackBox {
                contents,
            }
        }
    }

    mod cool {
        pub fn function() {
            println!("called `my1::cool::function()`");
        }
    }

    pub fn indirect_call() {
        print!("called `my1::indirect_call()`, that\n> ");
        self::function();
        function();
        self::cool::function();
        super::function();
        {
            use cool::function as root_function;
            root_function();
        }
    }
}

fn function() {
    println!("called `function()`");
}

mod deeply {
    pub mod nested {
        pub fn function() {
            println!("called `deeply::nested::function()`")
        }
    }
}

mod cool {
    pub fn function() {
        println!("called `cool::function()`");
    }
}

mod my1;

fn main() {
    // my1
    function();
    my::function();
    my::indirect_access();
    my::nested::function();
    // struct visibility
    let white_box = my::WhiteBox {contents:"public information"};
    println!("The white box contains: {}", white_box.contents);
    let _black_box = my::BlackBox::new("classified information");
    // use
    use deeply::nested::function as other_function;
    other_function();
    println!("Entering blocking");
    {
        use deeply::nested::function;
        function();
        println!("Leaving block");
    }
    function();
    // super and self
    my::indirect_call();
    // split
    my1::function();
    function();
    my1::indirect_access();
    my1::nested::function();
}