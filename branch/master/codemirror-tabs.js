/** Multiple document management using tabs for CodeMirror
 * (c) 2018 Imperial College London and Inria
 * Author: Thomas Wood
 * Licensed under the Apache 2.0 License
 */

{
  let freshDocIndex = 0;
  let freshDocName = () => "untitled" + ++freshDocIndex;

  let updateTabText = doc =>
    doc.tab.textContent = (doc.isClean() ? "" : "*") + doc.getName();

  let swapValidDoc = function(cm) {
    for (const doc of cm.state.docs.values()) {
      cm.swapDoc(doc);
      return;
    }
    cm.add(new CodeMirror.Doc());
  };

  let setActiveTab = doc => doc.tab.className = "file_item_current";

  let unsetActiveTab = doc => {
    if (doc.tab) {
      doc.tab.className = "file_item";
    }
  };

  CodeMirror.defineInitHook(cm => {
    cm.state.tabsDiv = document.createElement("div");
    cm.addPanel(cm.state.tabsDiv, {position: "after-top"});

    cm.state.docs = new Map();
    let doc = cm.getDoc();
    cm.addDoc(doc);
    setActiveTab(doc);

    cm.on("swapDoc", (cm, oldDoc) => {
      unsetActiveTab(oldDoc);
      setActiveTab(cm.getDoc());
    });
  });

  CodeMirror.defineExtension("addDoc", function(doc) {
    this.state.docs.set(doc.getName(), doc);

    if(!doc.tab) {
      doc.tab = document.createElement("span");
      doc.tab.className = "file_item";
      doc.tabEh = e => this.swapDoc(doc); // Stored so we can remove the handler later
      doc.tab.addEventListener("click", doc.tabEh);
    }
    updateTabText(doc);
    doc.on("change", (doc, _) => updateTabText(doc));

    this.state.tabsDiv.appendChild(doc.tab);
  });

  CodeMirror.defineExtension("addDocs", function(docs) {
    for(let doc of docs) {
      this.addDoc(doc)
    }
  });

  CodeMirror.defineExtension("setDocs", function(docs) {
    if (docs.length == 0) { throw new Error("At least one doc must be supplied to setDocs"); }

    const removedDocs = new Set(this.state.docs.values());
    docs.forEach(doc => removedDocs.delete(doc));

    // Clear out map to guarantee correct insertion order
    this.state.docs = new Map();
    this.addDocs(docs);
    this.swapDoc(docs[0]);

    return this.removeDocs(removedDocs);
  });

  CodeMirror.defineExtension(Symbol.iterator, function() {
    return this.state.docs.values();
  });

  CodeMirror.defineExtension("getDocs", function() {
    return [...this.state.docs.values()];
  });

  CodeMirror.defineExtension("getDocNames", function() {
    return new Set(this.state.docs.keys());
  });

  // doc may be a Doc or a name of a doc
  CodeMirror.defineExtension("removeDoc", function (doc, dontSwap) {
    if (typeof doc === "string") {
      doc = this.state.docs.get(doc);
    }
    if (doc === undefined) {
      return;
    }
    doc.tab.remove();
    doc.tab.removeEventListener("click", doc.tabEh);
    doc.tabEh = null;
    doc.tab = null;

    // Test that the doc being removed hasn't already been replaced in the map
    const name = doc.getName();
    if (this.state.docs.get(name) === doc) {
      this.state.docs.delete(name);
    }

    const editor = doc.getEditor();
    if (!dontSwap && editor) {
      swapValidDoc(editor);
    }

    return doc;
  });

  CodeMirror.defineExtension("removeDocs", function(docs) {
    const removedDocs = [];
    for(const doc of docs) {
      removedDocs.push(this.removeDoc(doc, true));
    }
    swapValidDoc(this);
    return removedDocs;
  });

  CodeMirror.defineExtension("swapDocByName", function(name) {
    return this.swapDoc(this.state.docs.get(name));
  });

  CodeMirror.defineExtension("markAllClean", function() {
    for (const doc of this) {
      doc.markClean();
    }
  });

  /****** Doc Extensions ********/
  // Note: doc.copy() does not copy the name.
  CodeMirror.defineDocExtension("setName", function(name) {
    return this.name = (name || freshDocName());
  });

  // Note: doc.copy() does not copy the name.
  CodeMirror.defineDocExtension("getName", function() {
    return this.name || this.setName();
  });

  const oldMarkClean = CodeMirror.Doc.prototype.markClean;
  CodeMirror.defineDocExtension("markClean", function() {
    oldMarkClean.call(this);
    const pos = {ch: 0, line: 0};
    CodeMirror.signal(this, "change", this, {from: pos, to: pos, text: [], removed: "", origin: ""});
  });
}
