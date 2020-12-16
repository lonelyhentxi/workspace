import "./styles.css";
import { useRef, useState } from "react";

function encodeUTF8(content) {
  const contentString = `${content}`;
  return escape(encodeURIComponent(contentString));
}

/**
 * @param name { string }
 * @param value { FormDataEntryValue }
 * @param boundary { string }
 * @param { Promise<string> }
 */
async function formDataEntryToPostBodyFragment({ name, value, boundary }) {
  const isBlob = value instanceof Blob || value instanceof File;
  if (!isBlob) {
    return (
      `--${boundary}\r\n` +
      `Content-Disposition: form-data; name="${encodeUTF8(name)}"\r\n\r\n` +
      `${encodeUTF8(value)}\r\n`
    );
  } else {
    const content = await new Promise((resolve) => {
      const reader = new FileReader();
      const readFn = () => {
        reader.removeEventListener("loadend", readFn);
        resolve(reader.result.split(",")[1]);
      };
      reader.addEventListener("loadend", readFn);
      reader.readAsDataURL(value);
    });
    return (
      `--${boundary}\r\n` +
      `Content-Disposition: form-data; name="${encodeUTF8(name)}"; filename="${
        value.name || "blob"
      }"\r\n` +
      `Content-Type: ${value.type || "application/octet-stream"}\r\n` +
      `Content-Transfer-Encoding: base64\r\n\r\n` +
      `${content}\r\n`
    );
  }
}

/**
 * @param formData { FormData }
 * @param boundary { string }
 * @param { Promise<string> }
 */
async function formDataToPostBody({ formData, boundary }) {
  const fragments = await Promise.all(
    [...formData.entries()].map(([name, value]) =>
      formDataEntryToPostBodyFragment({
        name,
        value,
        boundary
      })
    )
  );
  return `${fragments.join("")}--${boundary}--\r\n`;
}

export default function App() {
  const filesRef = useRef(null);
  const [formDataPath, setFormDataPath] = useState("");
  const [customPath, setCustomPath] = useState("");

  const handleSubmitForm = (isCustom) => async () => {
    if (!isCustom) {
      setFormDataPath("");
    } else {
      setCustomPath("");
    }
    /**
     * @type { File }
     */
    const file = filesRef.current?.files?.[0];
    if (file) {
      const chunkSize = 1024 * 1024;
      const allSize = file.size;
      const chunkNum = Math.ceil(allSize / chunkSize);
      const chunks = new Array(chunkNum).fill(0).map((v, i) => {
        const chunkIndex = i + 1;
        const chunkFile = file.slice(
          chunkSize * i,
          Math.min(chunkSize * (i + 1), allSize)
        );
        return {
          chunkIndex,
          chunkFile,
          chunkNum
        };
      });
      const initRes = await fetch(
        "http://127.0.0.1:9999/api/file/upload/init",
        {
          method: "POST",
          body: JSON.stringify({
            filename: file.name,
            mime: file.type,
            chunkNum: chunkNum
          }),
          headers: {
            "Content-Type": "application/json;charset=UTF-8",
            Accept: "application/json;charset=UTF-8"
          }
        }
      );
      if (!initRes.ok) {
        return false;
      }
      const initBody = await initRes.json();
      const uploadKey = initBody.key;
      if (!isCustom) {
        for (const c of chunks) {
          const fd = new FormData();
          fd.append("index", c.chunkIndex.toString());
          fd.append("file", c.chunkFile);
          fd.append("key", uploadKey);
          if (
            !(
              await fetch("http://127.0.0.1:9999/api/file/upload/chunk", {
                method: "POST",
                body: fd,
                headers: {
                  Accept: "application/json;charset=UTF-8"
                }
              })
            ).ok
          ) {
            return false;
          }
        }
      } else {
        for (const c of chunks) {
          const fd = new FormData();
          fd.append("index", c.chunkIndex.toString());
          fd.append("file", c.chunkFile);
          fd.append("key", uploadKey);
          const boundary = `----Boundary${Date.now()}${Math.floor(
            Math.random() * 1000000
          )}`;
          const bodyString = await formDataToPostBody({
            formData: fd,
            boundary
          });
          if (
            !(
              await fetch("http://127.0.0.1:9999/api/file/upload/chunk", {
                method: "POST",
                body: bodyString,
                headers: {
                  "Content-Type": `multipart/form-data; boundary=${boundary}`,
                  Accept: "application/json;charset=UTF-8"
                }
              })
            ).ok
          ) {
            return false;
          }
        }
      }
      const completeRes = await fetch(
        "http://127.0.0.1:9999/api/file/upload/complete",
        {
          method: "POST",
          body: JSON.stringify({
            key: uploadKey
          }),
          headers: {
            "Content-Type": "application/json;charset=UTF-8",
            Accept: "application/json;charset=UTF-8"
          }
        }
      );
      if (!completeRes.ok) {
        return false;
      }
      const completeBody = await completeRes.json();
      if (!isCustom) {
        setFormDataPath(completeBody.path);
      } else {
        setCustomPath(completeBody.path);
      }
    }
  };

  return (
    <div className="App">
      <input type="file" accept="image/*" ref={filesRef} />
      <button onClick={handleSubmitForm(false)}>提交表单</button>
      <button onClick={handleSubmitForm(true)}>提交自定义</button>
      <div>
        {formDataPath && (
          <img
            alt="formdata-img"
            src={`http://127.0.0.1:9999/${formDataPath}`}
          />
        )}
        {customPath && (
          <img alt="formdata-img" src={`http://127.0.0.1:9999/${customPath}`} />
        )}
      </div>
    </div>
  );
}
