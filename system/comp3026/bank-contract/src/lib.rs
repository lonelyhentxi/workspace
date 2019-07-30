use smart_contract::payload::Parameters;
use smart_contract::log;
use std::collections::{VecDeque, HashMap};
use rust_decimal::Decimal;
use std::ops::{SubAssign, AddAssign};
use std::str::FromStr;
use rust_decimal::prelude::*;
use smart_contract_macros::smart_contract as smart_contract_macro;
use std::fmt;
use hex::FromHex;

const PERMISSION_DENIED: &'static str = "permission denied";
const NOT_ENOUGH: &'static str = "not enough";
const NOT_EXISTS: &'static str = "not existed";
// const DECIMAL_LENGTH: usize = 64;

fn to_hex_string(bytes: [u8; 32]) -> String {
    let strs: Vec<String> = bytes.iter().map(|b| format!("{:02x}", b)).collect();
    strs.join("")
}

#[derive(Debug)]
struct Transaction {
    requester: [u8; 32],
    sender: [u8; 32],
    receiver: [u8; 32],
    amount: Decimal,
}

impl fmt::Display for Transaction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{},{},{}",
               to_hex_string(self.requester),
               to_hex_string(self.sender),
               to_hex_string(self.receiver),
               self.amount)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
enum Privilege {
    Admin = 0,
    Clerk = 1,
    Customer = 2,
}

impl fmt::Display for Privilege {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Privilege::Admin => write!(f, "admin"),
            Privilege::Clerk => write!(f, "clerk"),
            Privilege::Customer => write!(f, "customer"),
        }
    }
}

#[derive(Debug)]
struct Actor {
    privilege: Privilege,
    identity: [u8; 32],
    enabled: bool,
}

impl fmt::Display for Actor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{},{}", to_hex_string(self.identity), self.privilege, self.enabled)
    }
}

#[derive(Debug)]
struct TransferRequest {
    sender: [u8; 32],
    receiver: [u8; 32],
    amount: Decimal,
}

impl smart_contract::payload::Readable for TransferRequest {
    fn read_from(buffer: &[u8], pos: &mut u64) -> TransferRequest {
        let sender: [u8; 32] = smart_contract::payload::Readable::read_from(buffer, pos);
        let receiver: [u8; 32] = smart_contract::payload::Readable::read_from(buffer, pos);
        let amount_str_vec: Vec<u8> = smart_contract::payload::Readable::read_from(buffer, pos);
        let amount_str = String::from_utf8(amount_str_vec).unwrap();
        let amount = Decimal::from_str(&amount_str).unwrap();
        TransferRequest {
            sender,
            receiver,
            amount,
        }
    }
}

struct Bank {
    actors: HashMap<[u8; 32], Actor>,
    transactions: VecDeque<Transaction>,
}

struct BankUtil;

impl BankUtil {
    fn validate_clerk_editor(bank: &Bank, requester: &[u8; 32]) -> Result<(), String> {
        let requester_actor = BankUtil::get_actor(&bank, requester)?;
        match requester_actor.privilege {
            Privilege::Admin => Ok(()),
            _ => Err(PERMISSION_DENIED.into()),
        }
    }

    fn validate_customer_editor(bank: &Bank, requester: &[u8; 32]) -> Result<(), String> {
        let requester_actor = BankUtil::get_actor(bank, requester)?;
        match requester_actor.privilege {
            Privilege::Admin | Privilege::Clerk => Ok(()),
            _ => Err(PERMISSION_DENIED.into())
        }
    }

    fn get_actor<'a>(bank: &'a Bank, identity: &[u8; 32]) -> Result<&'a Actor, String> {
        match bank.actors.get(identity) {
            Some(actor) => Ok(actor),
            None => Err(PERMISSION_DENIED.into())
        }
    }

    fn balance(bank: &Bank, actor: &Actor) -> Decimal {
        let mut balance = BankUtil::actor_init_amount(&actor.privilege);
        for t in &bank.transactions {
            if t.sender == actor.identity {
                balance.sub_assign(t.amount);
            } else if t.receiver == actor.identity {
                balance.add_assign(t.amount);
            }
        }
        return balance;
    }

    fn actor_init_amount(privilege: &Privilege) -> Decimal {
        match privilege {
            Privilege::Admin => Decimal::max_value(),
            Privilege::Clerk => Decimal::zero(),
            Privilege::Customer => Decimal::zero(),
        }
    }
}

#[smart_contract_macro]
impl Bank {
    pub fn init(_params: &mut Parameters) -> Self {
        let mut bank = Self {
            actors: HashMap::new(),
            transactions: VecDeque::new(),
        };

        let root_actor = Actor {
            identity:
            <[u8; 32]>::from_hex(
                "400056ee68a7cc2695222df05ea76875bc27ec6e61e8e62317c336157019c405"
            ).unwrap(),
            privilege: Privilege::Admin,
            enabled: true,
        };
        bank.actors.insert(root_actor.identity, root_actor);
        bank
    }

    pub fn enable_clerk(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        BankUtil::validate_clerk_editor(&self, &requester)?;
        let target: [u8; 32] = params.read();
        let target_actor = (match BankUtil::get_actor(&self, &target) {
            Ok(actor) => {
                match actor.privilege {
                    Privilege::Clerk => Ok(Actor {
                        identity: target,
                        privilege: Privilege::Clerk,
                        enabled: true,
                    }),
                    _ => Err(PERMISSION_DENIED.to_string())
                }
            }
            Err(_) => Ok(Actor {
                identity: target,
                privilege: Privilege::Clerk,
                enabled: true,
            })
        })?;
        self.actors.insert(target, target_actor);
        Ok(())
    }

    pub fn disable_clerk(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        BankUtil::validate_clerk_editor(&self, &requester)?;
        let target: [u8; 32] = params.read();
        let target_actor = (match BankUtil::get_actor(&self, &target) {
            Ok(actor) => {
                match actor.privilege {
                    Privilege::Clerk => Ok(Actor {
                        identity: target,
                        privilege: Privilege::Clerk,
                        enabled: false,
                    }),
                    _ => Err(PERMISSION_DENIED.to_string())
                }
            }
            Err(_) => Err(NOT_EXISTS.to_string())
        })?;
        self.actors.insert(target, target_actor);
        Ok(())
    }

    pub fn enable_customer(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        BankUtil::validate_customer_editor(&self, &requester)?;
        let target: [u8; 32] = params.read();
        let target_actor = (match BankUtil::get_actor(&self, &target) {
            Ok(actor) => {
                match actor.privilege {
                    Privilege::Customer => Ok(Actor { identity: target, privilege: Privilege::Customer, enabled: true }),
                    _ => Err(PERMISSION_DENIED.to_string()),
                }
            }
            Err(_) => Ok(Actor { identity: target, privilege: Privilege::Customer, enabled: true })
        })?;
        self.actors.insert(target, target_actor);
        Ok(())
    }

    pub fn disable_customer(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        BankUtil::validate_customer_editor(&self, &requester)?;
        let target: [u8; 32] = params.read();
        let target_actor =
            (match BankUtil::get_actor(&self, &target) {
                Ok(actor) => {
                    match actor.privilege {
                        Privilege::Customer => Ok(Actor {
                            identity: target,
                            privilege: Privilege::Customer,
                            enabled: false,
                        }),
                        _ => Err(PERMISSION_DENIED.to_string()),
                    }
                }
                Err(_) => Err(NOT_EXISTS.to_string())
            })?;
        self.actors.insert(target, target_actor);
        Ok(())
    }

    pub fn transfer(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        let actor = BankUtil::get_actor(&self, &requester)?;
        if !actor.enabled {
            return Err(PERMISSION_DENIED.into());
        };
        let request: TransferRequest = params.read();
        (match actor.privilege {
            Privilege::Admin | Privilege::Clerk => Ok(request.sender),
            Privilege::Customer => if requester == request.sender { Ok(request.sender) } else { Err(PERMISSION_DENIED.to_string()) },
        })?;
        let balance = BankUtil::balance(&self, actor);
        if balance < request.amount {
            return Err(NOT_ENOUGH.into());
        }
        self.transactions.push_back(Transaction {
            requester,
            sender: request.sender,
            receiver: request.sender,
            amount: request.amount,
        });
        Ok(())
    }

    pub fn get_transactions(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        let actor = BankUtil::get_actor(&self, &requester)?;
        if !actor.enabled {
            return Err(PERMISSION_DENIED.into());
        }
        let target = (match actor.privilege {
            Privilege::Admin | Privilege::Clerk => Ok(params.read::<[u8; 32]>()),
            Privilege::Customer => {
                if params.read::<[u8; 32]>() == requester { Ok(requester) } else { Err(PERMISSION_DENIED.to_string()) }
            }
        })?;
        let mut target_transactions = VecDeque::new();
        for t in &self.transactions {
            if t.sender == target || t.receiver == target {
                target_transactions.push_back(t);
            }
        }
        log(&target_transactions.into_iter().map(Transaction::to_string)
            .collect::<Vec<String>>()
            .join("\n"));
        Ok(())
    }

    pub fn get_actors(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        let actor = BankUtil::get_actor(&self, &requester)?;
        if !actor.enabled {
            return Err(PERMISSION_DENIED.into());
        };
        match actor.privilege {
            Privilege::Admin | Privilege::Clerk => (),
            _ => return Err(PERMISSION_DENIED.into())
        };
        let mut actors = Vec::new();
        for a in self.actors.values() {
            actors.push(a);
        }
        log(&actors.into_iter().map(Actor::to_string)
            .collect::<Vec<String>>().join("\n"));
        Ok(())
    }

    pub fn get_actor(&mut self, params: &mut Parameters) -> Result<(), String> {
        let requester = params.sender;
        let actor = BankUtil::get_actor(&self, &requester)?;
        log(&actor.to_string());
        Ok(())
    }
}