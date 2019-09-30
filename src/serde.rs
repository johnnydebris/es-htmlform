use std::fmt;
use std::collections::HashMap;

use serde::de::{Deserialize, Deserializer, Visitor, MapAccess, SeqAccess};
use serde::ser::{Serialize, Serializer};

use crate::value::{Value, ValueMap};

// serde Deserialize
struct ValueMapVisitor;

impl<'de> Visitor<'de> for ValueMapVisitor {
    type Value = ValueMap;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a single value or list of values (strings)")
    }

    fn visit_map<M>(self, mut access: M)
            -> Result<Self::Value, M::Error>
            where M: MapAccess<'de> {
        let mut values: HashMap<String, Vec<Value>> = HashMap::new();
        while let Some((key, value)) =
                access.next_entry::<String, ValueVec>()? {
            values.insert(key, value.0);
        }

        Ok(ValueMap::new(values))
    }
}

impl <'de> Deserialize<'de> for ValueMap {
    fn deserialize<D>(deserializer: D)
            -> Result<Self, D::Error>
            where D: Deserializer<'de> {
        deserializer.deserialize_map(ValueMapVisitor {})
    }
}

// ValueVec is only used here to register a visitor that can deal with
// both vecs (multiple values) and strings (single values) as serialized
// data
struct ValueVec(Vec<Value>);

struct ValueVecVisitor;

impl<'de> Visitor<'de> for ValueVecVisitor {
    type Value = ValueVec;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a single value or list of values (strings)")
    }

    fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
            where M: SeqAccess<'de> {
        let mut values = vec![];
        while let Some(value) = access.next_element::<Value>()? {
            values.push(value);
        }

        Ok(ValueVec(values))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![Value::new(value)]))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![Value::new(&format!("{}", value))]))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![Value::new(&format!("{}", value))]))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![Value::new(&format!("{}", value))]))
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![]))
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![]))
    }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        Ok(ValueVec(vec![Value::new(if value {"on"} else {"off"})]))
    }
}

impl <'de> Deserialize<'de> for ValueVec {
    fn deserialize<D>(deserializer: D)
            -> Result<Self, D::Error>
            where D: Deserializer<'de> {
        deserializer.deserialize_any(ValueVecVisitor {})
    }
}

// ValueVisitor does the same as ValueVecVisitor, minus seq support
struct ValueVisitor;

impl<'de> Visitor<'de> for ValueVisitor {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a single value (string)")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> {
        Ok(Value::new(value))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        Ok(Value::new(&format!("{}", value)))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        Ok(Value::new(&format!("{}", value)))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E> {
        Ok(Value::new(&format!("{}", value)))
    }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        Ok(Value::new(if value {"on"} else {"off"}))
    }
}

impl <'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D)
            -> Result<Self, D::Error>
            where D: Deserializer<'de> {
        deserializer.deserialize_any(ValueVisitor {})
    }
}

// serde Serialize
impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        serializer.serialize_str(&self.as_str())
    }
}
