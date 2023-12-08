function RemoteFetch(url, opts) {
  return new Promise((resolve, reject) => {
    try {
      fetch(url, opts)
      .then(response => {
        if(response.redirected) window.location = response.url;
        else if (!response.ok) reject(response);
        else resolve(response);
      });
    } catch(e) {
      console.warn("Error in Remote Fetch", e);
      reject(e.message);
    } 
  });
}
self.Encryption = (() => {
  async function encrypt(message) {
    const nonce = sodium.randombytes_buf(sodium.crypto_secretbox_NONCEBYTES);
    const key = await loadKey();
    return sodium.to_hex(nonce) + "_" + sodium.to_hex(sodium.crypto_secretbox_easy(message, nonce, sodium.from_hex(key)));
  } 
  function decrypt(nonce_and_cipher, key) {
    let nonce_cipher = nonce_and_cipher.split("_");
    let nonce = sodium.from_hex(nonce_cipher[0]),
      cipher = sodium.from_hex(nonce_cipher[1]);
    return sodium.to_string(sodium.crypto_secretbox_open_easy(cipher, nonce, sodium.from_hex(key)));
  }
  function loadKey(quietly) {
    return new Promise((resolve, reject) => {
      let script = document.createElement("script");
      script.src = "config.js";
      script.onload = function () {
        resolve(mySecret());
        script.parentNode.removeChild(script);
        mySecret = undefined;
      };
      script.onerror = function(e) {
        if(!quietly) {
          new bootstrap.Tab("#pills-setting-tab").show();
          showModal("Error", '<code>config.js</code> not found. You will need to download <code>config.js</code> to <p class="mt-3"><code id="path">' +
          window.location.pathname.match(/.*(?=index.html$)/) +'</code><a onclick="navigator.clipboard.writeText($(\'#path\').text()).then()">' +
          '<img src="images/copy.svg"></a></p>' +
          'Click on "Settings" tab to download (and set up REDCap API Token).');
        }
        reject('"config.js" not found.');
      };
      script = document.documentElement.firstChild.appendChild(script);
    });
  }
  async function getSecret(secret, quietly) {
    const key = await loadKey(quietly).catch(e => {new Error("Cannot load key:  " + e.message)});
    if (!secret) return null;
    let txt = null;
    try {
      txt = decrypt(secret, key);
    } catch (e) {
      if (e.message.includes("wrong secret key")) {
        showModal("Secret Key Mismatch", `Unfortuneately the key in <code>config.js</code> does not decode the current REDCap API Token.
          <ol><li>The REDCap API Token will be unset</li><li>Reenter the REDCap API Token on the "Settings" tab</li></ol>`);
          config.RC.apikey = null;
          localStorage.setItem("config", JSON.stringify(config));
          new bootstrap.Tab("#pills-setting-tab").show();
      } else showModal("Key Error", e.message);
    }
    return txt;
  }
  function newKey() {
    $(".modal-footer").prepend(`<button id="confirmKey" class="btn btn-danger">Download</button>`);
    $("#confirmKey").click(async (e) => { 
      // get old key
      let key = sodium.to_hex(sodium.crypto_secretbox_keygen());
      // Reencrypt old apikey if present
      let apikey = config.RC.apikey ? await getSecret(config.RC.apikey, true).catch(e => {return null;}) : null;
      if (apikey !== null) {
        let nonce = sodium.randombytes_buf(sodium.crypto_secretbox_NONCEBYTES);
        config.RC.apikey = sodium.to_hex(nonce) + "_" + sodium.to_hex(sodium.crypto_secretbox_easy(apikey, nonce, sodium.from_hex(key)));
        localStorage.setItem("config", JSON.stringify(config));
      } else if (config.RC.apikey) {
        // the api key is set but using a secret we no longer have access to...
        config.RC.apikey = null;
        localStorage.setItem("config", JSON.stringify(config));
      } 
      let text = `function mySecret () {return "${ key }";}`;
      let element = document.createElement('a');
      element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
      element.setAttribute('download', "config.js");
      element.style.display = 'none';
      document.body.appendChild(element);
      element.click();
      document.body.removeChild(element);
        $("#myModal").modal("hide");
    });
    showModal("Continue?", "You must save this file as 'config.js' in the root directory with 'index.html' in order to access any authentication keys.");
    $("#myModal").on('hidden.bs.modal', e => { $("#confirmKey").remove(); });
  }
  return {
    encrypt,
    getSecret,
    newKey
  };
})();
var config = {};
const RemoteConfig = (() => {
  async function getConfig(){
    return await RemoteFetch("config").then(response => {return response.json()});
  }
  function save(config){
    return RemoteFetch("config", { method: "POST", headers: { "Content-Type": "application/json"}, body: JSON.stringify(config)});
  }
  return {
    getConfig: getConfig,
    save: save
  };
});
const LocalConfig = (() => {
  async function getConfig() {
    if(!localStorage.getItem("config")) {
      config = {
        usePID: true,
        pidName: "PID",
        xpertTZ:  "America/Sao_Paulo",
        debug: true,
        xpert: {
          '16': {
            use: true,
            ct: 40
          },
          '18_45': {
            use: true,
            ct: 32
          },
          P3: {
            use: true,
            ct: 30
          },
          P4: {
            use: false,
            ct: null
          },
          P5: {
            use: false,
            ct: null
          }
        }
      };
      config.version = version;
      config.RC = {
        api: null,
        apikey: null
      };
      localStorage.setItem("config", JSON.stringify(config));
    }
   return JSON.parse(localStorage.getItem("config"));
  }
  async function save() {
      // We have to await encrypts if this is set
    apikey = $("#apikey").val();
    if (apikey) {
      apikey = await Encryption.encrypt(apikey);
      config.RC.apikey = apikey;
    }
    localStorage.setItem("config", JSON.stringify(config));
    return;
  }
  return {
    getConfig: getConfig,
    save: save
  };
});
const RemoteData = (() => {
  function search(q, cb) {
    RemoteFetch("search?q=" + q)
    .then(response => response.json().then(cb))
    .catch(response => response.json().then(e => { showErr(e[0].msg, e[0].err, false);}));
  }
  async function updateCRF(sn) {
    RemoteFetch("crf",{body: JSON.stringify({sn: sn}), 
      method: "POST", headers : {"Content-Type": "application/json"}})
      .then(async (response) => {
        json = await response.json();
        json.forEach(e => {
          $("#ul" + e.cartridge_sn).text(dateFormat.format(new Date(e.uploaded)));
        });   
      })
      .catch(response => {response.json().then(msg => showErr(msg.msg, msg.err))});
  }
  async function getPID(sample_ids) {
    const response = await RemoteFetch("pid", {body: JSON.stringify({sampleid: sample_ids}), 
      method: "POST", headers : {"Content-Type": "application/json"}});
    if(response.ok) {
      const sn = await response.json();
      sn.forEach(x => $(`#sid${x.sample_id}`).text(x.pid));
      return(sn);
    }
  }
  async function deleteXpert(sn) {
    const response = await RemoteFetch("delete?sn=" + sn, {method: "DELETE"})
    return
  }
  function logout() {
    RemoteFetch("test?action=logout");
  }
  function deleteDB() {
    $(".modal-footer").prepend(`<button id="confirmDel" class="btn btn-danger">Confirm Delete</button>`);
    $("#myModal").on('hidden.bs.modal', e => { $("#confirmDel").remove(); });
    $("#confirmDel").click(async (el) => { 
      clearErr();
      $("#resTbl").addClass("d-none");
      $("#search").val("");
      const response = await RemoteFetch("test?action=deletedb")
      if(response.ok) {
        $("#myModal").modal("hide");
        showToast("Database Deleted");
      }
    });
    showModal("Confirm Delete DB?", "This cannot be undone. Consider backup!");
  }
  function getCSV(){
    window.location = "xpert";
  }
  return{
    search: search,
    getPID: getPID,
    deleteDB: deleteDB,
    deleteXpert: deleteXpert,
    logout: logout,
    updateCRF: updateCRF,
    getCSV: getCSV
  };
});
const LocalData = (() => {
  function initDB() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open("xpertdb", version);
      request.onsuccess = (e) => {
        resolve(e.target.result);
      };
      request.onerror = (e) => {
        console.error(e);
        reject(e);
      };
      request.onupgradeneeded = (e) => {
        db = e.target.result;
        const objStore = db.createObjectStore("xpert_results", { keyPath: "cartridge_sn" });
        objStore.createIndex("search", "search", { multiEntry: true , unique: false});
        objStore.createIndex("sample_id", "sample_id", {unique: false});
        objStore.transaction.oncomplete = (e) => {
          console.log("Database upgraded/created");
        };
      };
    });
  }
  async function write(xpert, update) {
    const db = await initDB();
    const transaction = db.transaction(["xpert_results"], "readwrite");
    const objStore = transaction.objectStore("xpert_results");
    return Promise.all(xpert.map(e => {
      return new Promise((resolve, reject) => {
        e.search = [e.sample_id, e.pid];
        const request = update !== undefined ? objStore.put(e) : objStore.add(e);
        request.onsuccess = (e) => resolve(e.target.result);
        request.onerror = (e) => reject(e.target.error);
      });
    }));
  }
  async function updatePID(sample_id, pid) {
    const db = await initDB();
    const obs = db.transaction("xpert_results", "readwrite").objectStore("xpert_results");
    const idx = obs.index("sample_id");
    const only = IDBKeyRange.only(sample_id);
    let result = idx.get(only);
    return new Promise((resolve, reject) => {
      result.onsuccess = e => {
        let xpert = e.target.result;
        xpert.pid = pid;
        xpert.search = [xpert.sample_id, xpert.pid];
        let put = obs.put(xpert);
        // this should be the cartridge_sn
        put.onsuccess = (p) => resolve(p.target.result);
      };
    });
  }
  async function crfDB(sn, id) {
    const db = await initDB();
    const obs = db.transaction("xpert_results", "readwrite").objectStore("xpert_results");
    let result = await new Promise((resolve, reject) => {
      const res = obs.get(sn);
      res.onsuccess = e => resolve(e.target.result);
      res.onerror = e => reject(e);
    });
    delete result.pdf;
    result.uploaded = new Date();
    result.crf_id = Number.parseInt(id);
    return new Promise((resolve, reject) => {
      res = obs.put(result);
      res.onsuccess = e => resolve({ sn: e.target.result, uploaded: result.uploaded });
      res.onerror = e => reject(e);
    });
  }
  function deleteDB() {
    $(".modal-footer").prepend(`<button id="confirmDel" class="btn btn-danger">Confirm Delete</button>`);
    $("#myModal").on('hidden.bs.modal', e => { $("#confirmDel").remove(); });
    $("#confirmDel").click((el) => { 
      const dbdel = indexedDB.deleteDatabase("xpertdb");
      dbdel.onerror = (e) => {console.log(e)};
      dbdel.onsuccess = (e) => { 
        clearErr();
        $("#resTbl").addClass("d-none");
        $("#search").val("");
        $("#myModal").modal("hide");
        showToast("Database Deleted");
      };
      dbdel.onblocked = (e) => {
        showErr("Database Delete Blocked: refresh browser and retry.", "No additional info");
        $("#myModal").modal("hide");
      };
        
    });
    showModal("Confirm Delete DB?", "This cannot be undone. Consider backup!");
    
  }
  function search(q, cb) {
    initDB().then(db => {
      const idx = db.transaction("xpert_results").objectStore("xpert_results").index("search");
      const range = IDBKeyRange.bound(q, q + '\uffff');
      let result = idx.openCursor(range);
      let results = [];
      result.onsuccess = e => {
        let cursor = e.target.result;
        if(cursor) {
          results.push(cursor.value);
          cursor.continue();
        } else {
          cb(results);
        }
      };
    });
  }
  async function dbGetAll(keys) {
    const db = await initDB();
    const os = db.transaction("xpert_results").objectStore("xpert_results");
    const xpert = await Promise.all(keys.map(key => {
      return new Promise((resolve, reject) => {
        const result = os.get(key);
        result.onsuccess = (e) => resolve(e.target.result);
        result.onerror = (e) => reject(e);
      });
    }));
    return xpert;
  }
  function getCSV() {
    initDB().then (db => {
      const obs = db.transaction("xpert_results").objectStore("xpert_results");
      let result = obs.openCursor();
      let xpert = [];
      result.onsuccess = (e) => {
        const cursor = e.target.result;
        if(cursor) {
          xpert.push(cursor.value);
          cursor.continue();
        } else {
          let keys = ['SAC_result', 'SAC_Ct', 'HPV_16_result', 'HPV_16_Ct', 'HPV_18_45_result', 'HPV_18_45_Ct', 'P3_result', 'P3_Ct', 'P4_result', 
            'P4_Ct', 'P5_result', 'P5_Ct', 'restrict_result', 'mod_ct_result', 'sample_id', 'test_result', 'status', 'error', 'error_message', 
            'start_time', 'end_time', 'instrument_sn', 'cartridge_sn', 'reagant_lot', 'notes', 'pid', 'uploaded'];
          xpert = calcXpert(xpert);
          xpert = xpert.map(x => {
            return keys.map(key => {
              return x[key] instanceof Date ? luxon.DateTime.fromJSDate(x[key]).toFormat("yyyy-MM-dd HH:mm:ss") : x[key]
            }).join(",");
          }).join("\n");
          let element = document.createElement('a');
          element.setAttribute('href', "data:text/csv;charset=utf-8," + encodeURI(keys.join(",") + "\n" + xpert));
          element.style.display = 'none';
          element.setAttribute('download', "xpert.csv");
          element.click();
          element.remove();
        }
      }
    });
  }
  function updateCRF(sn) {
    return REDCap.updateCRF(sn);
  }
  function getPID(sample_id) {
    return REDCap.getPID(sample_id);
  }
  async function deleteXpert(sn) {
    rc = await REDCap.clearCRF(sn);
    return new Promise(async (resolve) => {
      const db = await initDB();
      let result =  db.transaction("xpert_results", "readwrite").objectStore("xpert_results").delete(sn);
      result.onsuccess = e => resolve();
    });
  }
  return {
    dbGetAll: dbGetAll,
    search: search,
    initDB: initDB,
    write: write,
    deleteDB: deleteDB,
    getCSV: getCSV,
    updatePID: updatePID,
    updateCRF: updateCRF,
    crfDB: crfDB,
    getPID: getPID,
    deleteXpert: deleteXpert
  };
});
const LocalUtils = (() => {
  function backup(target) {
    target.disabled = true;
    REDCap.checkConf();
    let backup = {};
    backup.config = {};
    Object.assign(backup.config, config);
    delete backup.config.RC;
    backup.db = [];
    Data.initDB().then (db => {
      const obs = db.transaction("xpert_results").objectStore("xpert_results");
      let result = obs.openCursor();
      result.onsuccess = async (e) => {
        const cursor = e.target.result;
        if(cursor) {
          backup.db.push(cursor.value);
          cursor.continue();
        } else {
          // Convert pdfs to base64
          backup.db = await Promise.all(backup.db.map(async xpert => {
            return new Promise((resolve) => {
              if (xpert.pdf) {
                const blob = new Blob([xpert.pdf]);
                const reader = new FileReader();
                reader.onload = (event) => {
                  const dataUrl = event.target.result;
                  xpert.pdf = dataUrl.split(',')[1];
                  resolve(xpert);
                };
                reader.readAsDataURL(blob);
              } else {
                resolve(xpert);
              }
            });
          }));
          let data = {
            content : "fileRepository",
            action : "list",
            format : "json",
            returnFormat : "json"
          };
          const key = await Encryption.getSecret(config.RC.apikey);
          result = await REDCap.post(data, key)
          .catch(e => {
            showErr("Unable to get file repository", "REDCap Error: " + e);
          });
          let file = result.find(e => e.name === "Scrapert_Backup.json");
          if (file) {
            data.action = "delete";
            data.doc_id = file.doc_id;
            result = await REDCap.post(data, key, { dataType: "text"})
            .catch(e => {
              showErr("Unable to delete prior backup", "REDCap Error: " + e);
            });
          }
          data = new FormData();
          data.set("content", "fileRepository");
          data.set("action", "import");
          data.set("returnFormat", "json");
          data.set("file", new Blob([JSON.stringify(backup)], {type: "application/json"}), "Scrapert_Backup.json");
          result = await REDCap.post(data, key);
          showToast("Backup Complete (Backed up to REDCap as Scrapert_Backup.json)");
          target.disabled = false;
        }
      };
    });
  }
  async function restoreBackup(target) {
    target.disabled = true;
    REDCap.checkConf();
    let data = {
      content : "fileRepository",
      action : "list",
      format : "json",
      returnFormat : "json"
    };
    const key = await Encryption.getSecret(config.RC.apikey);
    result = await REDCap.post(data, key)
    .catch(e => {
      showModal("Unable to get file repository", "REDCap Error: " + e);
    });
    let file = result.find(e => e.name === "Scrapert_Backup.json");
    data = {
      content: "fileRepository",
      action: "export",
      doc_id: file.doc_id,
      returnFormat: "json"
    };
    file = await REDCap.post(data, key, {dataType: "json"})
      .catch(e => {
        showModal("Unable to download file", "REDCap Error: " + e.message);
      });
    // Convert pdfs from Base64
    file.db = await Promise.all(file.db.map(db => {
      return new Promise((resolve) => {
        if (db.pdf) {
          const dURL = "data:application/pdf;base64," + db.pdf;
          fetch(dURL).then(res => res.arrayBuffer())
            .then(buffer => {
              db.pdf = buffer;
              resolve(db);
            });
        } else resolve(db);
      });
    }));
    // Save the datq (keep our credentials!)
    const RC = config.RC;
    config = file.config;
    config.RC = RC;
    localStorage.setItem("config", JSON.stringify(config));
    Data.write(file.db, true);
    showToast("Backup Restored");
    target.disabled = false;
  }
  return {
    backup: backup,
    restoreBackup: restoreBackup
  }
});
const RemoteUtils = (() => {
  async function backup(target) {
    target.disabled = true;
    const result = await RemoteFetch("backup", {method: "POST"}).catch(async (r) => {
      console.error(r);
    });
    if (result.ok) {  
      fileinfo = await result.json();
      showToast("Backed up to REDCap: " + fileinfo.fileinfo);
    }
    target.disabled = false;
  }
  async function restoreBackup(target) {
    target.disabled = true;
    const result = await RemoteFetch("restore", {method: "POST"}).catch(async (r) => {
      console.error(r);
    });
    if (result.ok) {
      config = await Config.getConfig();
      showToast("Restored backup from REDCap");
    }
    target.disabled = false;
  }
  return {
    backup: backup,
    restoreBackup:  restoreBackup
  }
});