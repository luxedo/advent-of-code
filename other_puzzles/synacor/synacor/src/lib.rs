use std::ops::Deref;

pub struct VM {
    /* == architecture ==
     * - three storage regions
     *   - memory with 15-bit address space storing 16-bit values
     *   - eight registers
     *   - an unbounded stack which holds individual 16-bit values
     * - all numbers are unsigned integers 0..32767 (15-bit)
     * - all math is modulo 32768; 32758 + 15 => 5
     */
    memory: [u16; Self::ADDRESS_SPACE],
    register: [U15; Self::REGISTERS],
    stack: Vec<u16>,
    pc: U15,
    stdin: std::io::Stdin,
    running: bool,
}

impl VM {
    pub const ADDRESS_SPACE: usize = 1 << 15;
    pub const REGISTERS: usize = 8;
    pub fn new(program: Vec<u8>) -> Self {
        let mut vm = VM {
            memory: [0; Self::ADDRESS_SPACE],
            register: [0.into(); Self::REGISTERS],
            stack: Vec::new(),
            pc: 0.into(),
            stdin: std::io::stdin(),
            running: true,
        };
        let max_bytes = vm.memory.len() * 2;
        let bytes_to_copy = program.len().min(max_bytes);
        let (_, dest, _) = unsafe { vm.memory.align_to_mut::<u8>() };
        dest[..bytes_to_copy].copy_from_slice(&program);
        vm
    }

    #[inline]
    fn increment_pc(&mut self) {
        self.pc += U15::new_unchecked(1);
    }

    #[inline]
    fn read_addr(&self, addr: U15) -> u16 {
        self.memory[addr.value() as usize]
    }

    #[inline]
    fn write_addr(&mut self, addr: U15, value: u16) {
        self.memory[addr.value() as usize] = value;
    }

    #[inline]
    fn read_literal(&self, addr: U15) -> U15 {
        self.memory[addr.value() as usize].into()
    }

    #[inline]
    fn peek_number(&mut self) -> Number {
        self.read_addr(self.pc).into()
    }

    #[inline]
    fn pop_number(&mut self) -> Number {
        let num = self.read_addr(self.pc).into();
        self.increment_pc();
        num
    }

    #[inline]
    fn peek_register_index(&mut self) -> Register {
        match self.read_addr(self.pc).into() {
            Number::Literal(_) => panic!("Not a register"),
            Number::Register(register) => register,
        }
    }

    #[inline]
    fn pop_register_index(&mut self) -> Register {
        let reg = self.peek_register_index();
        self.increment_pc();
        reg
    }

    #[inline]
    fn set_register(&mut self, register: Register, value: U15) {
        self.register[register.0] = value;
    }

    #[inline]
    fn peek_value(&mut self) -> U15 {
        match self.read_addr(self.pc).into() {
            Number::Literal(value) => value,
            Number::Register(register) => self.peek_register(register),
        }
    }

    #[inline]
    fn pop_value(&mut self) -> U15 {
        let val = self.peek_value();
        self.increment_pc();
        val
    }

    #[inline]
    fn set_value(&mut self, value: U15) {
        self.set_value_at(self.pc, value);
    }

    #[inline]
    fn set_value_at(&mut self, addr: U15, value: U15) {
        self.memory[addr.0 as usize] = *value;
    }

    #[inline]
    fn peek_register(&self, register: Register) -> U15 {
        self.register[register.value()]
    }

    #[inline]
    fn peek_literal(&mut self) -> U15 {
        self.read_literal(self.pc)
    }

    #[inline]
    fn pop_literal(&mut self) -> U15 {
        let val = self.peek_literal();
        self.increment_pc();
        val
    }

    #[inline]
    fn pop_instruction(&mut self) -> Opcode {
        self.pop_literal().as_opcode()
    }

    #[inline]
    fn set_number(&mut self, number: Number, val: U15) {
        match number {
            Number::Literal(addr) => self.set_value_at(addr, val),
            Number::Register(register) => self.set_register(register, val),
        }
    }

    #[inline]
    fn with_set_number_3(&mut self, f: impl Fn(U15, U15) -> U15) {
        let a = self.pop_number();
        let b = self.pop_value();
        let c = self.pop_value();
        let val = f(b, c);
        self.set_number(a, val);
    }

    #[inline]
    fn with_set_number_2(&mut self, f: impl Fn(U15) -> U15) {
        let a = self.pop_number();
        let b = self.pop_value();
        let val = f(b);
        self.set_number(a, val);
    }

    pub fn diss(&self) {
        let mut i = 0;

        while i < self.memory.len() {
            let op = U15(self.read_addr((i as u16).into()));
            if op.0 > 21 {
                i += 1;
                continue;
            }
            let op = op.as_opcode();
            macro_rules! format_opcode {
                () => {
                    format!("0x{:02x} - {:?}", i, op)
                };
                ($num:expr) => {{
                    let mut s = format!("0x{:02x} - {:?}", i, op);
                    for j in 1..=$num {
                        s.push_str(&format!(" {}", self.read_addr(((i + j) as u16).into())));
                        i += 1;
                    }
                    s
                }};
            }
            let p = match op {
                Opcode::Halt => format_opcode!(),
                Opcode::Set => format_opcode!(2),
                Opcode::Push => format_opcode!(1),
                Opcode::Pop => format_opcode!(1),
                Opcode::Eq => format_opcode!(3),
                Opcode::Gt => format_opcode!(3),
                Opcode::Jmp => format_opcode!(1),
                Opcode::Jt => format_opcode!(2),
                Opcode::Jf => format_opcode!(2),
                Opcode::Add => format_opcode!(3),
                Opcode::Mult => format_opcode!(3),
                Opcode::Mod => format_opcode!(3),
                Opcode::And => format_opcode!(3),
                Opcode::Or => format_opcode!(3),
                Opcode::Not => format_opcode!(2),
                Opcode::Rmem => format_opcode!(2),
                Opcode::Wmem => format_opcode!(2),
                Opcode::Call => format_opcode!(1),
                Opcode::Ret => format_opcode!(),
                Opcode::Out => format_opcode!(1),
                Opcode::In => format_opcode!(1),
                Opcode::Noop => format_opcode!(),
            };

            println!("{}", p);
            i += 1;
        }
    }
}

impl Iterator for VM {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.pop_instruction();
        println!("0x{:02x} - {:?}", &self.pc.0-1, &i);
        match i {
            Opcode::Halt => return None,
            Opcode::Set => {
                let a = self.pop_register_index();
                let b = self.pop_value();
                self.set_register(a, b);
            }
            Opcode::Push => {
                let a = self.pop_value();
                self.stack.push(a.into());
            }
            Opcode::Pop => {
                let a = self.pop_number();
                let val = self.stack.pop().unwrap().into();
                self.set_number(a, val);
            }
            Opcode::Eq => self.with_set_number_3(|b, c| (if b == c { 1 } else { 0 }).into()),
            Opcode::Gt => self.with_set_number_3(|b, c| (if b > c { 1 } else { 0 }).into()),
            Opcode::Jmp => {
                let a = self.pop_value();
                self.pc = a;
            }
            Opcode::Jt => {
                if self.pop_value() != 0 {
                    let a = self.pop_value();
                    self.pc = a;
                } else {
                    self.increment_pc()
                }
            }
            Opcode::Jf => {
                if self.pop_value() == 0 {
                    let a = self.pop_value();
                    self.pc = a;
                } else {
                    self.increment_pc()
                }
            }
            Opcode::Add => self.with_set_number_3(|b, c| b + c),
            Opcode::Mult => self.with_set_number_3(|b, c| b * c),
            Opcode::Mod => self.with_set_number_3(|b, c| b % c),
            Opcode::And => self.with_set_number_3(|b, c| b & c),
            Opcode::Or => self.with_set_number_3(|b, c| b | c),
            Opcode::Rmem => {
                let a = self.pop_register_index();
                let b = self.pop_value();
                let val = self.read_literal(b);
                self.set_register(a, val);
            }
            Opcode::Wmem => {
                let a = self.pop_value();
                let b = self.pop_value();
                self.set_value_at(a, b);
            }
            Opcode::Call => {
                let a = self.pop_value();
                self.stack.push(self.pc.into());
                self.pc = a;
            }
            Opcode::Ret => match self.stack.pop() {
                Some(addr) => self.pc = addr.into(),
                None => return None,
            },
            Opcode::Not => self.with_set_number_2(|b| !b),
            Opcode::Out => {
                let a = self.pop_value();
                print!("{}", a.value() as u8 as char);
            }
            Opcode::In => {
                let mut line = String::new();
                let a = self.pop_value();
                match self.stdin.read_line(&mut line) {
                    Ok(_) => {
                        line.pop();
                        dbg!(&line);
                        for (i, c) in line.chars().enumerate() {
                            self.set_value_at(a + U15::new_unchecked(i as u16), (c as u16).into());
                        }
                    }
                    Err(_e) => panic!("Oh no error reading input"),
                }
            }
            Opcode::Noop => {}
        }

        Some(())
    }
}

impl std::fmt::Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "pc: {:?}", self.pc)?;
        for (i, reg) in self.register.iter().enumerate() {
            writeln!(f, "reg{}: {:?}", i, reg)?;
        }
        Ok(())
    }
}

/* == binary format ==
*  - each number is stored as a 16-bit little-endian pair (low byte, high byte)
*  - numbers 0..32767 mean a literal value
*  - numbers 32768..32775 instead mean registers 0..7
*  - numbers 32776..65535 are invalid
*  - programs are loaded into memory starting at address 0
*  - address 0 is the first 16-bit value, address 1 is the second 16-bit value, etc
*/

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
struct U15(u16);

impl U15 {
    pub const MAX: u16 = (VM::ADDRESS_SPACE - 1) as u16;
    pub const MASK: u16 = 0x7FFF;

    #[inline]
    pub fn new(value: u16) -> Self {
        Self(value & Self::MASK)
    }

    #[inline]
    pub fn new_unchecked(value: u16) -> Self {
        Self(value)
    }

    #[inline]
    pub fn value(&self) -> u16 {
        self.0
    }

    #[inline]
    fn as_opcode(&self) -> Opcode {
        unsafe { std::mem::transmute(self.value()) }
    }
}

impl Deref for U15 {
    type Target = u16;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<U15> for u16 {
    #[inline]
    fn from(v: U15) -> Self {
        v.0
    }
}
impl From<u16> for U15 {
    #[inline]
    fn from(v: u16) -> Self {
        U15(v & U15::MASK)
    }
}
impl PartialEq<u16> for U15 {
    fn eq(&self, other: &u16) -> bool {
        self.0 == *other
    }
}

macro_rules! impl_unary_op {
    ($trait:ident, $method:ident) => {
        impl std::ops::$trait for U15 {
            type Output = Self;
            fn $method(self) -> Self {
                Self::new(self.0.$method())
            }
        }
    };
}

macro_rules! impl_op {
    ($trait:ident, $method:ident, $native_method:ident) => {
        impl std::ops::$trait for U15 {
            type Output = Self;
            fn $method(self, rhs: Self) -> Self {
                Self::new(self.0.$native_method(rhs.0))
            }
        }
    };
}

macro_rules! impl_assign_op {
    ($trait:ident, $method:ident) => {
        impl std::ops::$trait for U15 {
            fn $method(&mut self, rhs: Self) {
                self.0.$method(rhs.0);
            }
        }
    };
}

impl_op!(Add, add, wrapping_add);
impl_assign_op!(AddAssign, add_assign);
impl_op!(Mul, mul, wrapping_mul);
// impl_assign_op!(MulAssign, mul_assign);
impl_op!(Rem, rem, wrapping_rem);
// impl_assign_op!(RemAssign, rem_assign);
impl_op!(BitAnd, bitand, bitand);
// impl_assign_op!(BitAndAssign, bitand_assign);
impl_op!(BitOr, bitor, bitor);
// impl_assign_op!(BitOrAssign, bitor_assign);
impl_unary_op!(Not, not);

#[derive(Debug)]
struct Register(usize);

impl Register {
    const MAX: u8 = 0x7;
    const MASK: u16 = 0x7;

    pub fn new(value: u16) -> Self {
        Self((value & Self::MASK) as usize)
    }

    fn value(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
enum Number {
    Literal(U15),
    Register(Register),
}

impl Number {
    const MAX_LITERAL: u16 = U15::MAX + 1;
    const MAX_REGISTER: u16 = Self::MAX_LITERAL + Register::MAX as u16 + 1;
}

impl From<u16> for Number {
    fn from(value: u16) -> Self {
        match value {
            0..Self::MAX_LITERAL => Self::Literal(U15::new(value)),
            Self::MAX_LITERAL..Self::MAX_REGISTER => Self::Register(Register::new(value)),
            Self::MAX_REGISTER.. => panic!("Invalid number"),
        }
    }
}

// == opcode listing ==
#[repr(u16)]
#[derive(Debug)]
enum Opcode {
    // halt: 0
    //   stop execution and terminate the program
    Halt = 0,
    // set: 1 a b
    //   set register <a> to the value of <b>
    Set = 1,
    // push: 2 a
    //   push <a> onto the stack
    Push = 2,
    // pop: 3 a
    //   remove the top element from the stack and write it into <a>; empty stack = error
    Pop = 3,
    // eq: 4 a b c
    //   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
    Eq = 4,
    // gt: 5 a b c
    //   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
    Gt = 5,
    // jmp: 6 a
    //   jump to <a>
    Jmp = 6,
    // jt: 7 a b
    //   if <a> is nonzero, jump to <b>
    Jt = 7,
    // jf: 8 a b
    //   if <a> is zero, jump to <b>
    Jf = 8,
    // add: 9 a b c
    //   assign into <a> the sum of <b> and <c> (modulo 32768)
    Add = 9,
    // mult: 10 a b c
    //   store into <a> the product of <b> and <c> (modulo 32768)
    Mult = 10,
    // mod: 11 a b c
    //   store into <a> the remainder of <b> divided by <c>
    Mod = 11,
    // and: 12 a b c
    //   stores into <a> the bitwise and of <b> and <c>
    And = 12,
    // or: 13 a b c
    //   stores into <a> the bitwise or of <b> and <c>
    Or = 13,
    // not: 14 a b
    //   stores 15-bit bitwise inverse of <b> in <a>
    Not = 14,
    // rmem: 15 a b
    //   read memory at address <b> and write it to <a>
    Rmem = 15,
    // wmem: 16 a b
    //   write the value from <b> into memory at address <a>
    Wmem = 16,
    // call: 17 a
    //   write the address of the next instruction to the stack and jump to <a>
    Call = 17,
    // ret: 18
    //   remove the top element from the stack and jump to it; empty stack = halt
    Ret = 18,
    // out: 19 a
    //   write the character represented by ascii code <a> to the terminal
    Out = 19,
    // in: 20 a
    //   read a character from the terminal and write its ascii code to <a>;
    //   it can be assumed that once input starts, it will continue until a newline is encountered;
    //   this means that you can safely read whole lines from the keyboard and trust that they will be fully read
    In = 20,
    // noop: 21
    //   no operation
    Noop = 21,
}
