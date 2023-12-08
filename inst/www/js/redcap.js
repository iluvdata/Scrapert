const LocalREDCap = (() => {
  async function getPID(sample_ids) {
    let data = {
      content: "record",
      format: "json",
      type: "flat",
      fields: "sample_id,pid",
      filterLogic:  `[sample_id]='${ sample_ids.join("' OR [sample_id]='") }'`,
      returnFormat: "json"
    };
    sn = [];
    results = await post(data)
    .catch(e => 
      showErr("Unable to get PID(s)", "REDCap Error: " + e.message)
    );
    return Promise.all(results.filter(e => e.pid ? true : false).map(e =>  {
        $("#sid" + e.sample_id).text(e.pid);
        return Data.updatePID(e.sample_id, e.pid);
    }));
  }
  async function clearCRF(sn) {
    let xpert = await Data.dbGetAll([sn]);
    xpert = xpert.filter(e => { return e.pid !== undefined });
    if (xpert.length === 0) return;
    let recid = xpert[0].crf_id;
    xpert = null;
    const key = await Encryption.getSecret(config.RC.apikey);
    let data = {
      content: "file",
      action: "delete",
      record: recid,
      field: "xpert_data",
      event: "",
      returnFormat: "json"
    };
    let result = await post(data, key, {dataType: "text"})
      .catch(e => console.warn("Unable to delete file: " + e));
    data = {
      content: "record",
      format: "json",
      type: "flat",
      overwriteBehavior: "overwrite",
      forceAutoNumber: "false",
      returnContent: "count",
      returnFormat: "json",
      data: JSON.stringify ([{
        record_id: recid,
        xpert_result_complete: "",
        xpert_result: "",
        xpert_timestamp: "",
        cartridge_sn: "",
        xpert_processed_by: ""
      }])
    };
    result = await post(data, key).catch(e => {throw "REDCap unable to clear record: " + e.responseJSON.error});
  }
  async function getRecordIDs(xpert, key) {
    let data = {
      content: "record",
      format: "json",
      type: "flat",
      fields: "record_id,pid",
      filterLogic:  "[pid]='" + xpert.map(e => e.pid).join("' OR [pid]='") + "'",
      returnFormat: "json"
    };
    return await post(data, key)
      .catch(e => 
        showErr("Unable to map pids to record_ids", "REDCap Error: " + e.message));
  } 
  async function updateCRF(sn) {
    xpert = await Data.dbGetAll(sn);
    xpert = xpert.filter(e => { return e.pid !== undefined });
    const key = await Encryption.getSecret(config.RC.apikey);
    let recids = await getRecordIDs(xpert, key);
    xpert = xpert.map(e => { 
      e.record_id =  recids.find(({pid}) => pid === e.pid).record_id;
      return e;
    });
    xp = xpert.map(e => {
      return {
        xpert_result: e.test_result,
        xpert_timestamp: e.end_time,
        record_id: e.record_id,
        cartridge_sn: e.cartridge_sn,
        xpert_processed_by: "ScrapertJS",
        xpert_result_complete: 2,
      };
    });
    let data = {
      content: "record",
      format: "json",
      type: "flat",
      overWriteBehavior: "normal",
      forceAutoNumber: "false",
      returnContent: "count",
      returnFormat: "json",
      data: JSON.stringify(xp)
    };
    const count = await post(data, key)
      .catch(e => 
        showErr("Error updating CRF", "REDCap Error: " + e.message));
    xp = undefined;
    data = new FormData();
    data.set("content", "file");
    data.set("action", "import");
    data.set("field", "xpert_data");
    data.set("event", "");
    data.set("returnFormat", "json");
    data.set("token", key);
    xpert.forEach(async function (e, i) {
      data.set("record", e.record_id);
      data.set("file", new Blob([e.pdf], {type: "application/pdf"}), e.sample_id + ".pdf");
      const result = await post(data, key)
      .catch(e => {
        showErr("Unable to upload file", "REDCap Error: " + e.message);
      });
      // delete the file from the database if we uploaded and release from memory
      delete xpert[i].pdf;
      let res = await Data.crfDB(e.cartridge_sn, e.record_id);
      $("#ul" + res.sn).text(dateFormat.format(res.uploaded));
    }); 
  }
  async function getPDF(record_id) {
    let data = {
      content: "file",
      action: "export",
      record: record_id,
      field: "xpert_data",
      event: "",
      returnFormat: "json"
    };
    function myXHR() {
      
    };
    const file = await post(data, undefined, {xhr: () => {
      const xhr = new XMLHttpRequest();
      xhr.onreadystatechange = () => {
        if (xhr.readyState == 2 && xhr.status == 200) xhr.responseType = "blob";
      };
      return xhr;
    }}).catch(e => {
        throw new Error("Unable to download PDF file: " + e);
      });
    return file;
  }
  async function post(data, key, options) {
    checkConf();
    if (key === undefined) key = await Encryption.getSecret(config.RC.apikey).catch(e => {
      throw new Error(e);
    });
    let x  = { url: config.RC.api};
    if (options !== undefined) x = {...x, ...options };
    if (data instanceof FormData) {
      x.contentType =  false;
      x.processData = false;
      x.dataType = "text";
      data.set("token", key);
    } else data = { ...data, token: key};
    x.data = data;
    return new Promise((resolve, reject) => {
      $.post(x)
        .done(result => resolve(result))
        .fail((jqXHR) => {
          reject("REDCap Error: " + (jqXHR.responseJSON ? jqXHR.responseJSON.error : JSON.parse(jqXHR.responseText).error)); 
        });
    });
  }
  function checkConf() {
    if(!hasConf()) {
      new bootstrap.Tab("#pills-setting-tab").show();
      showModal("REDCap API Not Configured", 'Please enter the REDCap API URL and/or API Token on the "Settings" tab');
      throw new Error("REDCap configuration missing");
    }
  }
  function hasConf () {
    if (config) {
      if (config.RC) {
        if (config.RC.api && config.RC.apikey) return true;
      }
    }
    return false;
  } 
  return {
    getPID: getPID,
    updateCRF: updateCRF,
    hasConf: hasConf,
    checkConf: checkConf,
    getPDF: getPDF,
    clearCRF: clearCRF,
    post: post
  };
});