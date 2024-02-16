const version = 1;

var usePID = false;
$( document ).ready(async () => {
  $("#fileform").submit(function(event) {
    event.preventDefault();
    event.stopPropagation();
    PDFTools.upload();
  });
  $("#settingsForm").submit(e => {
    e.preventDefault();
    e.stopPropagation();
    saveSettings();
  });
  // Capture refreshing the settings screen
  $("#pills-setting-tab").on("show.bs.tab", e => {
    getSettings();
  });
  const ser = await setupEnviroment();
  //use pids
  usePID = config.usePID;
  if (!config.usePID) {
    $("#pidcolumn").addClass("d-none");
  } else {
    $("#search").attr("placeholder", `Search by Sample ID or ${config.pidName}`)
    $("#pidcolumn").text(config.pidName);
  }

});
async function setupEnviroment() {
  const local = window.location.protocol === "file:";
  if (local) {
    function addScript(url, cb, type) {
      let script = document.createElement("script");
      script.src = url;
      if (type) script.type = type;
      script.onload = function () {
        if(cb) cb();
      };
      script = document.documentElement.firstChild.appendChild(script);
      return script;
    }
    addScript("js/sodium.js");
    addScript("https://cdn.jsdelivr.net/npm/luxon/build/global/luxon.min.js");
    addScript("https://mozilla.github.io/pdf.js/build/pdf.mjs", () => {
        var { pdfjsLib } = globalThis;
        // The workerSrc property shall be specified.
        pdfjsLib.GlobalWorkerOptions.workerSrc = "https://mozilla.github.io/pdf.js/build/pdf.worker.mjs";
    }, "module");
    addScript("https://cdn.jsdelivr.net/npm/pdf-lib@1.4.0/dist/pdf-lib.min.js");
    self.Config = LocalConfig();
    self.Data = LocalData();
    self.Utils = LocalUtils();
    self.PDFTools = LocalPDFTools();
    self.REDCap = LocalREDCap();
    $("#keyRow").removeClass("d-none");
  } else {
    self.Config = RemoteConfig();
    self.PDFTools = RemotePDFTools();
    self.Data = RemoteData();
    self.Utils = RemoteUtils();
  }
  config = await Config.getConfig();
  if(local) {
    if(config.debug) $("#utilbtns").append(`<button id="deletedbbtn" type="button" class="btn btn-warning" 
      onclick="Data.deleteDB()">Delete DB</button>`);
    // do we need config?
    function rcCheck() {
      if (!REDCap.hasConf()) {
        new bootstrap.Tab("#pills-setting-tab").show();
        showModal("REDCap API Not Configured", 'Please enter the REDCap API URL and/or API Token on the "Settings" tab');
      }
    }
    window.sodium = {
      onload: () => {
        Encryption.getSecret(config.RC.apikey).then(rcCheck);
      }
    }
  } else {
    if(!config.server) {
      const ws = new WebSocket(`ws://${document.location.host}`);
      ws.onopen = (e) => {
        $("#utilbtns").append('<button id="statusbtn" type="button" class="btn btn-success ms-2">Exit</button></div>');
        $("#statusbtn").click((e) => {
            ws.send("shutdown");
          });
      };
      ws.onclose = (e) => {
        $("#statusbtn").removeClass("btn-success btn-warning")
          .addClass("btn-danger").text("Disconnected").off("click").click((e) => {
            showModal("Scapert Offline", "Please close this window and relaunch the app.");
        });
      };
    }
    if(config.debug) $("#utilbtns").prepend(`<button type="button" class="btn btn-warning" 
      onclick="Data.deleteDB()">Delete DB</button>`).prepend(`<button type="button" class="btn btn-secondary me-2" 
      onclick="Data.logout()">Logout</button>`);
  }
}
$(document).on('change', '.file-input', function() {
  let filesCount = $(this)[0].files.length;
  let textbox = $(this).prev();
  // Clear errs, table, and search box
  clearErr();
  $("#resTbl").addClass("d-none");
  $("#search").val("");
  for(let i=0; i < filesCount; i++) {
    let f = $(this)[0].files[i];
    if (f.type !== "application/pdf") {
      showErr(`Selected files must be all pdfs. The file "${f.name}" is not a pdf.`);
      $("#fileform").trigger("reset");
      return;
    }
  }
  if (filesCount > 0 ) {
    $("#processbtn").prop("disabled", false);
    if (filesCount === 1) {
      var fileName = $(this).val().split('\\').pop();
      textbox.text(fileName);
    } else {
      textbox.text(filesCount + ' files selected');
    }
  } else {
    $("#processbtn").prop("disabled", true);
    textbox = $(this);
  }
});
function calcXpert(xpert) {
  let ct = {}, use = {};
  let keys = ["HPV_16", "HPV_18_45", "P3", "P4", "P5"];
  keys.forEach(e => {
    channel = config.xpert[e.replaceAll("HPV_", "")];
    // Assign max cutoffs for the channel.
    ct[e] = channel.ct !== null ? Number.parseInt(channel.ct) : (e.startsWith("HPV") ? 40 : 38);
    use[e] = channel.use;
  });
  // Modify every row
  return xpert.map(e => {
    e.restrict_result = keys.some(k => { return e[`${ k }_result`] === "POS" && use[k] }) ? "POS" : "NEG";
    e.mod_ct_result = keys.some(k => { return e[`${ k }_ct`] != 0 && e[`${ k }_ct`] < ct[k] }) ? "POS" : "NEG";
    return e;
  });
}
function search(e){
  let val = e.value;
  if (val.length > 1) {
    Data.search(val, result => { updateTable(result); });
  } else {
    $("#resTbl").addClass("d-none");
  }
}
function del(sn, sampleId) {
  $(".modal-footer").prepend(`<button id="confirmDel" class="btn btn-danger"  disabled>Confirm Delete</button>`);
  showModal("Confirm Delete?", `<p>Delete sample <span id="sampleId">${sampleId}</span>?</p><p>Type the sample id below to confirm</p>
      <input type="text" class="form-control" id="sampleidconf" maxlength="${sampleId.length}">`);
  $("#confirmDel").click((e) => confDel(sn));
  $("#myModal").on('hidden.bs.modal', e => { $("#confirmDel").remove(); });
  $("#sampleidconf").keyup(e => {
    $("#confirmDel").attr("disabled", e.target.value.toLowerCase() != $("#sampleId").text().toLowerCase()); 
    }).trigger("focus");
}
function confDel(sn) {
  Data.deleteXpert(sn).then(() => {
    showToast("Sample Successfully Deleted");
    $("#resTbl").addClass("d-none");
    $("#search").val("");
  });
  $("#myModal").modal("hide"); 
}
function updateTable(data) {
  data = calcXpert(data);
  tbody = $("tbody");
  tbody.empty();
  if (data.length > 0) {
    for (let d in data) {
      tbody.append(`<tr><td><a href="#" class="link-underline-danger"
          onClick="del('${data[d].cartridge_sn }', '${data[d].sample_id}');">${data[d].sample_id}</a></td>
        <td ${ usePID ? "" : "class='d-none'"} id="sid${data[d].sample_id}">
          ${data[d].pid === undefined ? "<a onclick=\"lookupPID(['" + data[d].sample_id + "'])\">" +  
            "<img src=\"images/search.svg\" alt=\"Lookup PID\"/></a>" : data[d].pid }</td>
        <td>${data[d].cartridge_sn}</td>
        <td>${data[d].error}</td>
        <td>${data[d].test_result}</td>
        <td>${data[d].restrict_result}</td>
        <td>${data[d].mod_ct_result}</td>
        <td>${data[d].SAC_result === undefined ? "-": data[d].SAC_result}</td>
        <td>${data[d].HPV_16_result}</td>
        <td>${data[d].HPV_18_45_result}</td>
        <td>${data[d].P3_result}</td><td>${data[d].P4_result}</td><td>${data[d].P5_result}</td>
        <td><a onclick="PDFTools.getPDF('${data[d].cartridge_sn}')"><img src="images/file-earmark-pdf.svg" alt="View PDF"></a>
            <a onclick="PDFTools.dlPDF('${data[d].cartridge_sn}')"><img src="images/download.svg" alt="Download PDF"></a></td>
        <td id="ul${data[d].cartridge_sn}">${data[d].uploaded === undefined ? 
          "<a onclick=\"Data.updateCRF(['" + data[d].cartridge_sn + "']);\">" +  
          "<img src=\"images/cloud-upload.svg\" alt=\"Import\"/></a>" : dateFormat.format(new Date(data[d].uploaded))}</td></tr>`);
    }
  } else tbody.append("<tr><td colspan='13' class='text-middle'>No results found</td></tr>");
  $("#resTbl").removeClass("d-none");
}
const dateFormat = new Intl.DateTimeFormat("en-US", { dateStyle: "short", timeStyle: "short" });
function processErr(jqXHR, modal) {
  // did we get an ajax HTTP 401 response?
  if(jqXHR.status == 401) {
    showModal("Not Authenticated", "You must log in again to access this resource");
    $("#myModal").on('hidden.bs.modal', e => { window.location = jqXHR.responseJSON.url });
  }  else {
    if (modal === undefined) showErr(jqXHR.responseJSON[0].msg, jqXHR.responseJSON[0].err);
    else showModal(modal.title, modal.msg);
  }
}
function showErr(msg, err, stack) {
  const msgBox = $("#msgBox");
  msg = `<div class="alert alert-danger" id="msg" style="cursor: pointer"
                    onclick='showModal("Error", "${err.replaceAll("\"","\\\"").replaceAll("'","&apos;")}");'>${msg}</div>`
  if (stack || stack == undefined) msgBox.append(msg);
  else msgBox.html(msg);
  $("#msgRow").removeClass("d-none");
}
function clearErr() {
  $("#msgRow").addClass("d-none");
  $("#msgBox").empty();
}
function lookupPID(sampleId, stackErr) {
  if (!stackErr) clearErr();
  Data.getPID(sampleId);
}
function checkbox(label, name, checked) {
  return `<div class="md-3 row">
          <label for="${ name }" class="col-md-3 col-form-label text-end fw-bold">${ label }</label>
          <div class="col-md-9"><input type="checkbox" class="form-check-input" name="${ name }" id="${name}" value="true" 
              ${ checked ? "checked" : ""}></div></div>`;
}
function passcode(label, name, required) {
  return `<div class="md-3 row">
          <label for="${ name }" class="col-md-3 col-form-label text-end fw-bold">${ label }</label>
          <div class="col-md-9"><input type="password" class="form-control" name="${ name }" id="${ name }" value="" 
              ${ required ? "required" : ""} placeholder="Leave blank to keep current value"></div></div>`;
}
function getSettings() {
  sTab = $("#settingsDiv");
  sTab.empty();
  sTab.append(`<div class="mb-3 row">
    <div> class="col-md-3 text-end fw-bold">Version</div>
    <div class="col-md-9">${ config.version }</div></div>`);
  sTab.append(`<div class="mb-3 row">
    <label for="xpertTZ" class="col-md-3 col-form-label text-end fw-bold">Time Zone of Xpert Machine</label>
    <div class="col-md-9"><select class="form-select" name="xpertTZ" id="xpertTZ">
      ${ Intl.supportedValuesOf("timeZone").map((o) => "<option " + (o === config.xpertTZ ? "selected" : "") + ">" + o + "</option>").join("") }
    </select></div></div>`);
  sTab.append(checkbox("Debug Mode?", "debug", config.debug));
  sTab.append('<div class="md-3 row"><div class="col-md fw-bold text-center">REDCap Settings</div></div>');
  sTab.append(`<div class="md-3 row">
          <label for="api" class="col-md-3 col-form-label text-end fw-bold">REDCap API URL</label>
          <div class="col-md-9"><input type="text" class="form-control" name="api" id="api" value="${ config.RC && config.RC.api ? config.RC.api : ""}" 
             required placeholder="https://..."></div></div>` + 
             passcode("REDCap API Token", "apikey", !config.RC || !config.RC.apikey));
  
  sTab.append('<div class="md-3 row"><div class="col-md fw-bold text-center">Modified Xpert Settings</div></div>');
  for (let x in config.xpert) {
    sTab.append(checkbox(`Use ${x}?`, `use${x}`, config.xpert[x].use)); 
    sTab.append(`<div class="mb-3 row">
          <label for="ct${ x }" class="col-md-3 col-form-label text-end fw-bold">Xpert ${ x } Ct</label>
          <div class="col-md-9"><input type="number" class="form-control" id="ct${ x }" name="ct${ x }" 
            value="${ config.xpert[x].ct !== null ? config.xpert[x].ct : "" }" placeholder="Leave blank to ignore channel" min="0" max="40"></div></div>`);
  } 
}
function saveSettings() { 
  savebtn = $("#settingsForm input[type='submit']").attr("disabled", true);
  $("#settingsForm input:not(.btn)[id!='apikey'],select").each((i, el) => {
    if(el.name === "api") {
      if (!config.RC) config.RC = {};
      config.RC.api = el.value;
    } else if(!el.name.match(/^(ct|use).*/)) {
      config[el.name] = el.type === "checkbox" ? el.checked :
        (el.value === "" ? null : el.value);
    } else {
      name = el.name.replace(/^(ct|use)/, "");
      if(el.name.startsWith("ct")) config.xpert[name].ct = el.value === "" ? null : el.value
      else config.xpert[name].use = el.checked
    }
  });
  config.version = version;
  Config.save(config).then(() => {
    showToast("Settings Saved");
    savebtn.attr("disabled", false);
  }).catch(err => showModal("Unable to Save Settings", err));
}
function showModal(title, body) {
  $("#myModalLabel").text(title);
  $(".modal-body").html(body);
  const myModal = new bootstrap.Modal("#myModal");
  myModal.show();
}
function showToast(msg) {
  $(".toast-body").text(msg);
  const myToast = new bootstrap.Toast(".toast");
  myToast.show();
}