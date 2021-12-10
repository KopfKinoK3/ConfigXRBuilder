const readTextFile = window.__TAURI__.fs.readTextFile
const writeFile = window.__TAURI__.fs.writeFile

const docPath = '/Users/tkumlehn/Documents/configdocs'

   /// GET FILE

const fetchDocumentByFileName = (fileName) => {

  const filePath = docPath + "/" + fileName
  return readTextFile(filePath, {})
     .then(str => app.ports.infoForElm.send({tag: "GotFile", data: { fileName: fileName, content: str } }))

}

console.log("Im OK!")

app.ports.infoForOutside.subscribe(msg => {

    console.log("OUTSIDE")

    switch(msg.tag) {

        case "AskForClipBoard":
            console.log("AskForClipBoard")
            navigator.clipboard.readText()
              .then(text => {
                app.ports.infoForElm.send({tag: "GotClipboard", data: text})
              })
              .catch(err => {
                console.error('!JS! Failed to read clipboard: ', err);
              });

             break;

        case "WriteToClipboard":
            console.log("GotClipboard")
            navigator.permissions.query({name: "clipboard-write"}).then(result => {
              if (result.state == "granted" || result.state == "prompt") {
                updateClipboard(JSON.stringify(msg.data))
              }
            });
  
            break;
  

        //case "AskForFileList":
        //    
        //    getManifest()
        //    
        //    break;

        case "AskForFile":

            console.log("AskForFile")

            var fileName = msg.data

            console.log("File name", fileName)

            fetchDocumentByFileName(fileName)

           break;

        //case "CreateFile":
        //
        //    console.log("Here is CreateFile")
        //
        //    var document = msg.data
        //
        //    console.log("CREATE DOC", document.fileName)
        //    
        //    createFile(document)
        //    break;


        case "WriteFile":

            console.log("Here is WriteFile")

            var document = msg.data

            console.log("DOC", document.fileName)

            writeFile_(document)

            break;

        /*
        case "WriteMetadata":

            var document = msg.data

            console.log("Write metadata: ", document.fileName)

            writeMetadata(document)

              break;

        case "Highlight":

            var id = "#".concat(msg.data.id)
            var lastId = msg.data.lastId

            var element = document.querySelector(id)
            if (element != null) {
                  element.classList.add("highlight")
            } else {
                  console.log("!JS! Add: could not find id", id)
            }

            var lastElement = document.querySelector(lastId)
            if (lastElement != null) {
                  lastElement.classList.remove("highlight")
            } else {
                  console.log("!JS! Remove: could not find last id",lastId)
            }

            break;

        case "DeleteFileFromLocalStorage":

            localStorage.removeItem(msg.data);
            var fileList = filesInLocalStorage()
            app.ports.infoForElm.send({tag: "GotFileList", data:  fileList})
  
            break;
        */
  
    }

    /*
    function getManifest() {

      const path = docPath + '/manifest.yaml'

      const sendManifest = (value) => app.ports.infoForElm.send({tag: "GotFileList", data:  load(value)})

      return readTextFile(path,  {}).then(value => sendManifest(value))
    }
    */

    function updateClipboard(newClip) {
      console.log("updateClipboard")
      navigator.clipboard.writeText(newClip).then(function() {
      }, function() {
        console.log ("!JS! Clipboard write failed");
      });
    }
    
    function writeFile_(document) {

        const pathToFile = docPath + '/' + document.fileName

        writeFile({file: pathToFile, contents: document.content})
    }

    /*
    function writeMetadata(document) {

        const pathToManifest = docPath + '/manifest.yaml'

        const metadata = { fileName: document.fileName, id: document.id}

        // s and t are metadata: source and target
        const changeMetadata = (s, t ) =>
           s.id == t.id ?  Object.assign({}, s) : t

        // Update the item in the manifest m with id == s.id with the value s
        const updateManifest = (s, m) => m.map((t) => changeMetadata(s, t))

        readTextFile(pathToManifest,  {})
             .then(value => load(value))
             .then(m => updateManifest(metadata, m))
             .then(m => safeDump(m))
             .then(contents => writeFile({file: pathToManifest, contents: contents}))

    }

    function createFile(document) {

        const metadata = {fileName: document.fileName, id: document.id}

        const pathToManifest = docPath + '/manifest.yaml'

        writeFile_(document)

        const append = (item, array) => {

           array.push(item)

           return array
           }

        readTextFile(pathToManifest,  {})
             .then(value => load(value))
             .then(m => append(metadata, m))
             .then(m => safeDump(m))
             .then(contents => writeFile({file: pathToManifest, contents: contents}))

    }

    function getFileFromLocalStorage(fileName) {
        return JSON.parse(localStorage.getItem(fileName))
    }
    */

})
