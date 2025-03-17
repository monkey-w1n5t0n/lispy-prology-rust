# Risper: A Lisp-like Essence of Rust

This document explores what Rust might look like if reimagined with a minimal core and Lisp-like syntax similar to Clojure. The language (let's call it "Risper") preserves Rust's key concepts: ownership, borrowing, lifetimes, and type safety, while adopting Clojure's S-expression syntax (including vector and map literals), homoiconic syntactic macros, .

## Basic Syntax

### Variables and Bindings

**Risper:**
```clojure
(let x 5)
(let! y 10)
(set! y (+ y 1))
```

**Rust:**
```rust
let x = 5;
let mut y = 10;
y = y + 1;
```

Note how `let` doesn't have local-only scope. Is this a good thing? Is this a bad thing? We'll find out!

### Functions

**Risper:**
```clojure
(defn add [^i32 x ^i32 y] -> i32
  (+ x y))
  
(defn main []
  (println! (add 5 10)))
```

**Rust:**
```rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    println!("{}", add(5, 10));
}
```

## Ownership and Borrowing

### Ownership Transfer

**Risper:**
```clojure
(defn take-ownership [^String s] -> ()
  (println! s))
  
(defn main []
  (let s (String::from "hello"))
  (take-ownership s)
  ;; s is no longer valid here
)
```

**Rust:**
```rust
fn take_ownership(s: String) {
    println!("{}", s);
}

fn main() {
    let s = String::from("hello");
    take_ownership(s);
    // s is no longer valid here
}
```

### Borrowing

**Risper:**
```clojure
(defn calculate-length [^&String s] -> usize
  (s.len))
  
(defn main []
  (let s (String::from "hello"))
  (let len (calculate-length &s))
  (println! "The length of '{s}' is {len}"))
```

**Rust:**
```rust
fn calculate_length(s: &String) -> usize {
    s.len()
}

fn main() {
    let s = String::from("hello");
    let len = calculate_length(&s);
    println!("The length of '{}' is {}", s, len);
}
```

### Mutable References

**Risper:**
```clojure
(defn change [^&mut String s] -> ()
  (s.push_str ", world"))
  
(defn main []
  (let mut s (String::from "hello"))
  (change &mut s)
  (println! s))
```

**Rust:**
```rust
fn change(s: &mut String) {
    s.push_str(", world");
}

fn main() {
    let mut s = String::from("hello");
    change(&mut s);
    println!("{}", s);
}
```

## Lifetimes

**Risper:**
```clojure
(defn longest [^&'a str x ^&'a str y] -> &'a str
  (if (> x.len y.len) x y))
  
(defn main []
  (let s1 "long string is long")
  (let s2 "xyz")
  (let result (longest s1 s2))
  (println! "The longest string is {result}"))
```

**Rust:**
```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}

fn main() {
    let s1 = "long string is long";
    let s2 = "xyz";
    let result = longest(s1, s2);
    println!("The longest string is {}", result);
}
```

## Data Structures

### Structs

**Risper:**
```clojure
(defstruct User
  (username String)
  (email String)
  (sign_in_count u64)
  (active bool))
  
(defn main []
  (let mut user (User
                  :username (String::from "someone")
                  :email (String::from "someone@example.com")
                  :sign_in_count 1
                  :active true))
  (set! user.email (String::from "another@example.com")))
```

**Rust:**
```rust
struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

fn main() {
    let mut user = User {
        username: String::from("someone"),
        email: String::from("someone@example.com"),
        sign_in_count: 1,
        active: true,
    };
    
    user.email = String::from("another@example.com");
}
```

### Enums and Pattern Matching

**Risper:**
```clojure
(defenum Result
  (Ok T)
  (Err E))
  
(defn divide [^f64 numerator ^f64 denominator] -> (Result f64 String)
  (if (= denominator 0.0)
    (Err "Cannot divide by zero")
    (Ok (/ numerator denominator))))
    
(defn main []
  (let result (divide 9.0 3.0))
  (match result
    ((Ok value) (println! "Success: {value}"))
    ((Err message) (println! "Error: {message}"))))
```

**Rust:**
```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}

fn divide(numerator: f64, denominator: f64) -> Result<f64, String> {
    if denominator == 0.0 {
        Err(String::from("Cannot divide by zero"))
    } else {
        Ok(numerator / denominator)
    }
}

fn main() {
    let result = divide(9.0, 3.0);
    match result {
        Ok(value) => println!("Success: {}", value),
        Err(message) => println!("Error: {}", message),
    }
}
```

## Traits

**Risper:**
```clojure
(deftrait Summary
  (defn summarize [self] -> String))
  
(defstruct NewsArticle
  (headline String)
  (content String)
  (author String))
  
(impl Summary for NewsArticle
  (defn summarize [self] -> String
    (str "{self.headline}, by {self.author}")))
    
(defn main []
  (let article (NewsArticle
                 :headline (String::from "Breaking News")
                 :content (String::from "Something happened")
                 :author (String::from "Jane Doe")))
  (println! (article.summarize)))
```

**Rust:**
```rust
trait Summary {
    fn summarize(&self) -> String;
}

struct NewsArticle {
    headline: String,
    content: String,
    author: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {}", self.headline, self.author)
    }
}

fn main() {
    let article = NewsArticle {
        headline: String::from("Breaking News"),
        content: String::from("Something happened"),
        author: String::from("Jane Doe"),
    };
    println!("{}", article.summarize());
}
```

## Memory Safety with Option

**Risper:**
```clojure
(defn find_first [^Vec<i32> items ^i32 target] -> (Option usize)
  (let mut i 0)
  (while (< i (items.len))
    (if (= (items.get i) target)
      (return (Some i)))
    (set! i (+ i 1)))
  None)
  
(defn main []
  (let v [1 2 3 4 5])
  (let index (find_first v 3))
  (match index
    ((Some i) (println! "Found at index {i}"))
    (None (println! "Not found"))))
```

**Rust:**
```rust
fn find_first(items: Vec<i32>, target: i32) -> Option<usize> {
    let mut i = 0;
    while i < items.len() {
        if items[i] == target {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn main() {
    let v = vec![1, 2, 3, 4, 5];
    let index = find_first(v, 3);
    match index {
        Some(i) => println!("Found at index {}", i),
        None => println!("Not found"),
    }
}
```

## Closures

**Risper:**
```clojure
(defn main []
  (let add_one (fn [^i32 x] -> i32 (+ x 1)))
  (let result (add_one 5))
  (println! "The result is: {result}")
  
  (let expensive_calculation (memoize (fn [^u32 intensity] -> u32
                                        (println! "calculating slowly...")
                                        (thread::sleep (Duration::from_secs 2))
                                        intensity)))
  (expensive_calculation 1)  ;; slow
  (expensive_calculation 1)) ;; fast, uses cached result
```

**Rust:**
```rust
fn main() {
    let add_one = |x: i32| -> i32 { x + 1 };
    let result = add_one(5);
    println!("The result is: {}", result);
    
    use std::thread;
    use std::time::Duration;
    use std::collections::HashMap;
    
    struct Cacher<T>
    where
        T: Fn(u32) -> u32,
    {
        calculation: T,
        values: HashMap<u32, u32>,
    }
    
    impl<T> Cacher<T>
    where
        T: Fn(u32) -> u32,
    {
        fn new(calculation: T) -> Cacher<T> {
            Cacher {
                calculation,
                values: HashMap::new(),
            }
        }
        
        fn value(&mut self, arg: u32) -> u32 {
            match self.values.get(&arg) {
                Some(v) => *v,
                None => {
                    let v = (self.calculation)(arg);
                    self.values.insert(arg, v);
                    v
                }
            }
        }
    }
    
    let mut expensive_calculation = Cacher::new(|intensity| {
        println!("calculating slowly...");
        thread::sleep(Duration::from_secs(2));
        intensity
    });
    
    expensive_calculation.value(1);  // slow
    expensive_calculation.value(1);  // fast, uses cached result
}
```

## Macros

**Risper:**
```clojure
(defmacro vec! [& elements]
  `(let mut v (Vec::new))
   ~@(map #`(v.push ~%) elements)
   v)
   
(defn main []
  (let v (vec! 1 2 3 4 5))
  (println! "Vector: {v:?}"))
```

**Rust:**
```rust
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

fn main() {
    let v = vec![1, 2, 3, 4, 5];
    println!("Vector: {:?}", v);
}
```

This exploration shows how Rust's key concepts of ownership, borrowing, and type safety could be represented in a Lisp-like syntax while maintaining Rust's semantic model.