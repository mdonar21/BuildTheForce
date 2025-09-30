document.getElementById("getHallOfFame").onclick = function() {
  Shiny.setInputValue("getHallOfFame", Math.random());
};

// Listen for server messages
Shiny.addCustomMessageHandler("hallOfFameData", function(data) {
  let table = document.getElementById("hallOfFame");
  table.innerHTML = "";
  data.Call.Sign.forEach((name, i) => {
    let row = table.insertRow();
    row.insertCell(0).innerText = name;
    row.insertCell(1).innerText = data["Total Failure Points"][i];
    row.insertCell(2).innerText = data["Total Losses"][i];
  });
});

// Send tech slider value to Shiny
document.getElementById("tech").oninput = function() {
  Shiny.setInputValue("techLevel", this.value);
};
