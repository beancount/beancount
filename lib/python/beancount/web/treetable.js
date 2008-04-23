/*
author: Bojan Mihelac <bojan@mihelac.org>
http://source.mihelac.org
*/

var treetable_rowstate = new Array();
var treetable_callbacks = new Array();

function treetable_hideRow(rowId) {
  el = document.getElementById(rowId);
  el.style.display = "none";
}

function treetable_showRow(rowId) {
  el = document.getElementById(rowId);
  el.style.display = "";
}

function treetable_hasChildren(rowId) {
  res = document.getElementById(rowId + '_0');
  return (res != null);
}

function treetable_getRowChildren(rowId) {
  el = document.getElementById(rowId);
  var arr = new Array();
  i = 0;
  while (true) {
    childRowId = rowId + '_' + i;
    childEl = document.getElementById(childRowId);
    if (childEl) {
      arr[i] = childRowId;
    } else {
      break;
    }
    i++;
  }
  return (arr);
}

function treetable_toggleRow(rowId, state, force) {
  var rowChildren;
  var i;
  // open or close all children rows depend on current state
  force = (force == null) ? 1 : force; 
  if (state == null) {
    row_state = ((treetable_rowstate[rowId]) ? (treetable_rowstate[rowId]) : 1) * -1;
  } else {
    row_state = state;
  }
  rowChildren = treetable_getRowChildren(rowId);
  if (rowChildren.length == 0) return (false);
  for (i = 0; i < rowChildren.length; i++) {
    if (row_state == -1) {
      treetable_hideRow(rowChildren[i]);
      treetable_toggleRow(rowChildren[i], row_state, -1);
    } else {
      if (force == 1 || treetable_rowstate[rowId] != -1) {
        treetable_showRow(rowChildren[i]);
        treetable_toggleRow(rowChildren[i], row_state, -1);
      }
    }
  }
  if (force == 1) {
    treetable_rowstate[rowId] = row_state;
    treetable_fireEventRowStateChanged(rowId, row_state);
  }
  return (true);
}

function treetable_fireEventRowStateChanged(rowId, state) {
  if (treetable_callbacks['eventRowStateChanged']) {
    callback = treetable_callbacks['eventRowStateChanged'] + "('" + rowId + "', " + state + ");";
    eval(callback);
  }
}

function treetable_collapseAll(tableId) {
  table = document.getElementById(tableId);
  rowChildren = table.getElementsByTagName('tr');
  for (i = 0; i < rowChildren.length; i++) {
    if (index = rowChildren[i].id.indexOf('_')) {
      // do not hide root elements
      if(index != rowChildren[i].id.lastIndexOf('_')) {
        rowChildren[i].style.display = 'none';
      }
      if (treetable_hasChildren(rowChildren[i].id)) {
        treetable_rowstate[rowChildren[i].id] = -1;
        treetable_fireEventRowStateChanged(rowChildren[i].id, -1);
      }
    }
  }
  return (true);
}    

function treetable_expandAll(tableId) {
  table = document.getElementById(tableId);
  rowChildren = table.getElementsByTagName('tr');
  for (i = 0; i < rowChildren.length; i++) {
    if (index = rowChildren[i].id.indexOf('_')) {
      rowChildren[i].style.display = '';
      if (treetable_hasChildren(rowChildren[i].id)) {
        treetable_rowstate[rowChildren[i].id] = 1;
        treetable_fireEventRowStateChanged(rowChildren[i].id, 1);
      }
    }
  }
  return (true);
}


/* Added folder icons. */

treetable_callbacks['eventRowStateChanged'] = 'treetable_eventRowChanged';

function treetable_eventRowChanged(rowId, state) {
  img = document.getElementById(rowId).getElementsByTagName('td')[0].getElementsByTagName('img')[0];
  if (state == 1) {
    img.src = '/folder_open.png';
  } else {
    img.src = '/folder_closed.png';
  }
  return (true);
}  



