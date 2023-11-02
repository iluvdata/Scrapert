$(document).on('change', '.file-input', function() {
        
  var filesCount = $(this)[0].files.length;
  
  var textbox = $(this).prev();
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
function upload() {
  clearErr();
  let files = $("#xpertfiles").get(0);
  let formData = new FormData();
  for (let i = 0; i < files.files.length; i++) {
    const file = files.files[i];
    if(file.type !== "application/pdf") throw "Must be pdf";
    formData.append("file" + i, file);
  }
  $.ajax({
    url: "upload",
    method: "POST",
    data: formData,
    processData: false,
    contentType: false,
  }).done(
    function(data) {
      if(Object.keys(data.message).length > 0) showErr(data.message);
      updateTable("resTbl", data.results);
      // Extract Search ID
      let sample_id = [];
      for (let s in data.results) {
        sample_id.push(data.results[s].sample_id);
      }
      // Now try to lookup PIDs
      lookupPID(sample_id, true);
      // Now try to upload
      let id = [];
      for (let s in data.results) {
        id.push(data.results[s].id);
      }
      uploadCRF(id, true);
    }
  );
  $(".file-message").text("or drag and drop files here");
  $("#processbtn").prop("disabled", true);
  $("#fileform").trigger("reset");
}
function search(e){
  let val = e.value;
  if (val.length > 1) {
    $.get(`search?q=${val}`)
    .done(
      function(data) {
        updateTable("resTbl", data);
      }
    )
    .fail(function(jqXHR) {
      showErr(jqXHR.responseJSON);
    });
  } else {
    $("#resTbl").addClass("d-none");
  }
}
function updateTable(tbl, data) {
  tbody = $("tbody");
  tbody.empty();
  if (data.length > 0) {
    for (let d in data) {
      tbody.append(`<tr><td>${data[d].sample_id}</td>
        <td ${ usePID ? "" : "class='d-none'"} id="sid${data[d].sample_id}">
          ${data[d].pid === undefined ? "<a href=\"#\" onclick=\"lookupPID('" + data[d].sample_id + "');\">" +  
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
        <td>
          <a href="pdf?id=${data[d].id}" target="xpertpdf"><img src="images/file-earmark-pdf.svg" alt="View"/></a>
          <a href="pdf?id=${data[d].id}&dl=y"><img src="images/download.svg" alt="Download"/></a>
        </td>
        <td id="ul${data[d].id}">${data[d].uploaded === undefined ? 
          "<a href=\"#\" onclick=\"uploadCRF(" + data[d].id + ");\">" +  
          "<img src=\"images/cloud-upload.svg\" alt=\"Import\"/></a>" : dateFormat.format(new Date(data[d].uploaded + " GMT"))}</td></tr>`);
    }
  } else tbody.append("<tr><td colspan='13' class='text-middle'>No results found</td></tr>");
  $("#" + tbl).removeClass("d-none");
}
const dateFormat = new Intl.DateTimeFormat("en-US", { dateStyle: "short", timeStyle: "short" });
function showErr(msg) {
  let msgBox = $("#msgBox");
  msgBox.append(`<div class="alert alert-danger" id="msg" style="cursor: pointer"
                    onclick='showModal("Error", "${msg.err.replaceAll("\"","\\\"").replaceAll("'","&apos;")}");'>${msg.msg}</div>`);
  $("#msgRow").removeClass("d-none");
}
function clearErr() {
  $("#msgRow").addClass("d-none");
  $("#msgBox").empty();
}
function lookupPID(sampleId, stackErr) {
  if (!stackErr) clearErr();
  $.post({
    url: "pid", 
    data: JSON.stringify({ "sampleid" : sampleId }),
    contentType: "application/json"
  })
  .done(function(data) {
    for(let d in data) {
      $(`#sid${data[d].sample_id}`).text(data[d].pid);
    }
  }).fail(function(jqXHR) {
    showErr({ msg : jqXHR.responseJSON.msg, err :  jqXHR.responseJSON.err });
  });
}
function uploadCRF(id, stackErr) {
  if (!stackErr) clearErr();
  $.post({
    url: "crf", 
    data: JSON.stringify({ "id" : id }),
    contentType: "application/json"
  })
  .done(data => {
    for(let x in data) {
      $(`#ul${data[x].id}`).text(dateFormat.format(new Date(data[x].uploaded + " UTC")));
    }
  }).fail(function(jqXHR) {
    showErr({ msg : jqXHR.responseJSON.msg, err :  jqXHR.responseJSON.err });
  });
}
function getSettings() {
  $.get("settings")
  .done(data => {
    sTab = $("#settingsDiv");
    sTab.empty();
    for (let x in data) {
      if (x !== "xpert") {
        if(data[x].opts !== undefined) {
          sTab.append(`<div class="mb-3 row">
              <label for="${ x }" class="col-md-3 col-form-label text-end fw-bold">${ data[x].label }</label>
              <div class="col-md-9"><select class="form-select" name="${ x }">
                ${ data[x].opts.map((o) => "<option " + (o == data[x].value ? "selected" : "") + ">" + o + "</option>").join("") }
              </select></div></div>`);
         } else {
            sTab.append(`<div class="mb-3 row">
              <label for="${ x }" class="col-md-3 col-form-label text-end fw-bold">${ data[x].label }</label>
              <div class="col-md-9">${ 
              (data[x].type !== "password" ?
                '<input type="' + data[x].type + '" class="form-control" name="' + x + '" value="' +
                  (data[x].value !== undefined ? data[x].value : "") + '"' +
                  (data[x].required | data[x].needed ? "required" : "") + '>' 
              :
                '<input type="password" class="form-control" name="' + x  + '"' +
                (data[x].needed ? "required placeholder='Required'" +
                    "style='border-color: var(--bs-form-invalid-border-color);'" 
                  : 
                    'placeholder="Leave blank to keep current value"')+ ">")
              }
              </div></div>`);
         }
      }
    }
    sTab.append('<div class="mb-3 row"><div class="col-md-12 fw-bold text-center">Modified Xpert Settings</div></div>');
    for (let x in data.xpert) {
      sTab.append(`<div class="mb-3 row">
            <label for="use${ x }" class="col-md-3 col-form-label text-end fw-bold">Use Xpert ${ x }?</label>
            <div class="col-md-9"><input type="checkbox" class="form-check-input" name="use${ x }" value="true" 
                ${ data.xpert[x].use ? "checked" : ""}></div></div>`);
      sTab.append(`<div class="mb-3 row">
            <label for="ct${ x }" class="col-md-3 col-form-label text-end fw-bold">Xpert ${ x } Ct</label>
            <div class="col-md-9"><input type="number" class="form-control" name="ct${ x }" 
              value="${ data.xpert[x].ct}" placeholder="Leave blank to ignore channel" min="0" max="40"></div></div>`);
    } 
  })
  .fail((jqXHR, textStatus, errorThrow) => {
    showModal("Unable to get settings", `${textStatus} ${errorThrow}`);
  });
}
function saveSettings() {
  data = $("#settingsForm").serializeArray();
  settings = {};
  // Convert to object
  for (let x in data) {
    settings[data[x].name] = data[x].value;
  }
  $.post({
    url: "settings",
    data: JSON.stringify ({setting :  settings}),
    contentType: "application/json"
  }).done((data) => {
    showToast("Settings Saved.")
    getSettings();
  }).
  fail((jqXHR, textStatus, errorThrow) => {
    showModal("Unable to save settings.",  `${textStatus} ${errorThrow}`);
  });
}
function showModal(title, body) {
  $("#myModalLabel").text(title);
  $(".modal-body").text(body);
  const myModal = new bootstrap.Modal("#myModal");
  myModal.show();
}
function showToast(msg) {
  $(".toast-body").text(msg);
  const myToast = new bootstrap.Toast(".toast");
  myToast.show();
}