use std::fs;
use std::error::Error;
use std::path::{Path,PathBuf};
use std::collections::HashMap;
use std::iter::FromIterator;
#[macro_use]
extern crate simple_error;
#[macro_use]
extern crate lazy_static;
use regex::{Regex,Captures};

type BoxResult<T> = Result<T,Box<dyn Error>>;

lazy_static! {
    static ref TASK_NAME_PATTERN: Regex = 
        Regex::new(r"^(\w+)_(\d+)\.([\w\.]+)$").unwrap();
    static ref EXT_MAPPING: 
    HashMap<&'static str,&'static str>
    = HashMap::from_iter(
        vec![
            ("rs","rust"),
            ("sh","bash"),
            ("c","c"),
            ("h","c"),
            ("hpp","cpp"),
            ("cpp","cpp"),
            ("cxx","cpp"),
            ("hxx","cpp"),
            ("cs","csharp"),
            ("go","golang"),
            ("java","java"),
            ("js","javascript"),
            ("ts","typescript"),
            ("kt","kotlin"),
            ("kts","kotlin"),
            ("sql","mysql"),
            ("php","php"),
            ("py","python"),
            ("rb","ruby"),
            ("scala","scala"),
            ("swift","swift"),
            ("agda","agda"),
            ("bf","brainfuck"),
            ("clj","clojure"),
            ("cljs","clojure"),
            ("cljc","clojure"),
            ("coffee","coffeescript"),
            ("litcoffee","coffeescript"),
            ("v","coq"),
            ("cr","crystal"),
            ("dart","dart"),
            ("exs","elixir"),
            ("elm","elm"),
            ("erl","erlang"),
            ("hrl","erlang"),
            ("factor","factor"),
            ("f","fortran"),
            ("for","fortran"),
            ("f90","fortran"),
            ("fs","fsharp"),
            ("fsi","fsharp"),
            ("fsx","fsharp"),
            ("fsscript","fsharp"),
            ("groovy","groovy"),
            ("hs","haskell"),
            ("idr","idris"),
            ("jl","julia"),
            ("lua","lua"),
            ("nasm","asm"),
            ("nim","nim"),
            ("m","object-c"),
            ("ml","ocaml"),
            ("mli","ocaml"),
            ("ps1","powershell"),
            ("purs","purescript"),
            ("r","r"),
            ("rkt","racket"),
            ("re","reason"),
            ("vb","vb"),
            ("css","css"),
            ("d","d"),
            ("chpl","chapel"),
            ("lisp","lisp"),
            ("pl","perl"),
            ("scss","scss"),
            ("less","less"),
            ("sass","sass")
        ].into_iter()
        );
}

struct TaskMeta {
    path: PathBuf,
    id: i32,
    name: String,
    lang: String,
}

impl TaskMeta {
    pub fn create(path: PathBuf, file_name: &str) -> BoxResult<TaskMeta> {
        let (id, name,lang) = TaskMeta::analyse_metadata(&file_name)?;
        Ok(TaskMeta {
            path,
            id,
            name,
            lang
        })
    }

    pub fn mapping_ext(ext: &str) -> String {
        match EXT_MAPPING.get(ext) {
            Some(v) => v.to_string(),
            None => "unknown".to_string()
        }
    }

    /** 
     * Returns (task_id, task_name, task_lang) if succeed
     */
    pub fn analyse_metadata(file_name: &str) -> BoxResult<(i32,String,String)> {
        let caps = TASK_NAME_PATTERN.captures(file_name).ok_or("cap error")?;
        let name = caps.get(1).ok_or("cannot cap task name")?.as_str().to_string().replace("_"," ");
        let id = caps
            .get(2)
            .ok_or("cannot cap task number")?
            .as_str()
            .to_string()
            .parse::<i32>()?;
        let lang = TaskMeta::mapping_ext(&caps.get(3).ok_or("cannot cap ext name")?.as_str().to_string());
        Ok((id, name, lang))
    }

    pub fn to_table_row(&self) -> String {
        let norm_path = self.path.iter().map(|v| v.to_string_lossy().to_string()).collect::<Vec<String>>().join("/");
        format!("| {} | [{}]({}) | {} |", 
            self.id, self.name, norm_path, self.lang).to_string()
    }
}

fn find_all_tasks(p: &Path) -> BoxResult<Vec<BoxResult<TaskMeta>>> {
    let mut tasks = vec![];
    if !p.is_dir() {
        bail!(format!("\"{}\" is not directory", p.display()));
    }
    let mut tag_dirs = vec![];
    for entry in fs::read_dir(p)? {
        let entry = entry?;
        let sub_path = entry.path();
        if sub_path.is_dir() {
            let dir_name_str = sub_path
                .to_str()
                .unwrap_or("");
            if dir_name_str.ends_with("_tag") {
                tag_dirs.push(sub_path);
            }
        }
    }
    for dir_path in tag_dirs {
        for entry in fs::read_dir(dir_path)? {
            let item_entry = entry?;
            let item_path = item_entry.path();
            if item_path.is_file() {
                let item_name = item_path.file_name()
                    .map(|f| f.to_string_lossy())
                    .unwrap_or_else(|| "".into());
                if TASK_NAME_PATTERN.is_match(&item_name) {
                    tasks.push(TaskMeta::create(item_path.clone(), &item_name))
                }
            }
        }
    }
    Ok(tasks)
}

fn generate_table(tasks: Vec<BoxResult<TaskMeta>>) -> (String,Vec<Box<dyn Error>>) {
    let mut res = vec!["| # | Task | Language |\n|---| ----- | -------- |".to_string()];
    let mut task_sort = vec![];
    let mut errs = vec![];
    for task in tasks {
        match task {
            Ok(t) => task_sort.push(t),
            Err(e) => errs.push(e)
        }
    }
    task_sort.sort_by_key(|t| t.id);
    res.extend(task_sort.iter().map(&TaskMeta::to_table_row));
    (res.join("\n"), errs)
}

fn main() -> BoxResult<()> {
    let src_path = PathBuf::from("./src");
    let tasks = find_all_tasks(&src_path)?;
    let (table, error_task_meta) = generate_table(tasks);
    let mut contents = fs::read_to_string("./README.md")?;
    contents = Regex::new(r"<!--anker-start-->[^<>]+<!--anker-end-->").unwrap()
        .replace_all(&contents, |_: &Captures| { 
            format!("<!--anker-start-->\n{}\n<!--anker-end-->", table)
        })
        .to_string();
    fs::write("./README.md", &contents.as_bytes())?;
    for e in error_task_meta {
        eprintln!("{:?}", e);
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_task_name_pattern() {
        assert!(
            TASK_NAME_PATTERN
            .is_match("serialize_and_deserialize_binary_tree_297.rb")
        );
        assert!(
            TASK_NAME_PATTERN
            .is_match("serialize_and_deserialize_binary_tree_297.rs")
        );
        assert!(!
            TASK_NAME_PATTERN
            .is_match("mod.rs")
        );
    }

    #[test]
    fn test_task_name_cap() {
        let caps = TASK_NAME_PATTERN.captures("serialize_and_deserialize_binary_tree1_297.d.ts").unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(),"serialize_and_deserialize_binary_tree1");
        assert_eq!(caps.get(2).unwrap().as_str(),"297");
        assert_eq!(caps.get(3).unwrap().as_str(),"d.ts");
    }

    #[test]
    fn test_analyze_metadata() {
        let (id, name, lang) =  TaskMeta::analyse_metadata("serialize_and_deserialize_binary_tree1_297.d.ts").unwrap();
        assert_eq!((id,name,lang),(297,"serialize and deserialize binary tree1".to_string(),"unknown".to_string()));
        let (_, _, lang) =  TaskMeta::analyse_metadata("serialize_and_deserialize_binary_tree1_297.rs").unwrap();
        assert_eq!(lang, "rust");
    }

    #[test]
    fn test_to_table_row() {
        let task = TaskMeta::create(PathBuf::from(".\\src\\graph_tag\\course_schedule_207.rs"), "course_schedule_207.rs").unwrap();
        assert_eq!(task.to_table_row(), "| 207 | [course schedule](./src/graph_tag/course_schedule_207.rs) | rust |".to_string());
    }

    #[test]
    fn test_find_all_tasks() {
        let tasks = find_all_tasks(Path::new("./src"));
        assert!(!tasks.unwrap().is_empty());
    }
}