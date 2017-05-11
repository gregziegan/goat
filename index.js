// flags
var isMac = navigator.userAgent.indexOf('Mac OS X') != -1;
var client = window.ZAFClient ? ZAFClient.init() : null;
var isInZendeskAppContext = !!client;

var app = Elm.Main.embed(document.getElementById("root"), { isMac: isMac, inZendesk : isInZendeskAppContext });
var zendeskExportId;
var b64ImageDict = {};

// ports
app.ports.exportToImage.subscribe(function (imageId) {
  zendeskExportId = imageId;
  window.requestAnimationFrame(exportImage);
});

app.ports.listenForUpload.subscribe(function () {
  var target = document.querySelector('.droparea');
  target.addEventListener("dragover", function (e) {
    e.preventDefault();
  });
  target.addEventListener("drop", function (e) {
    e.preventDefault();
    var file = e.dataTransfer.files[0]
    if (!file.type.match(/image.*/)) {
      return;
    }
    loadImage(file)
      .then(function(loadedImage) {
        if (loadedImage === null) {
          alert('does not support this image type, please provide a PNG file');
        } else {
          app.ports.newImage.send(loadedImage);
        }
      })
  }, true);
});

app.ports.requestImages.subscribe(function () {
  if (isInZendeskAppContext) {
    readInImagesFromZendesk();
  } else {
    var goats = [
      { id: '0', url: 'images/goat.jpg', width: 235, height: 276, originalWidth: 639, originalHeight: 751},
      { id: '1', url: 'images/goat2.jpg', width: 294, height: 220, originalWidth: 800, originalHeight: 600}
    ];
    app.ports.setImages.send(goats);
    Array.prototype.forEach.call(goats, loadImageDataIntoCache);
  }
});

app.ports.selectText.subscribe(function (id) {
  var textarea = document.getElementById(id);
  if (textarea) {
    textarea.select();
  }
})

// zendesk listeners
if (isInZendeskAppContext) {
  client.invoke('resize', { height: 320, width: 800 });
  client.on('pane.activated', readInImagesFromZendesk);
  client.on('pane.deactivated', function() {
    app.ports.reset.send(null);
  })
}

var DOMURL = window.URL || window.webkitURL || window;

// helper functions
function loadImageDataIntoCache(imageObject) {
  var newImg = new Image();
  newImg.width = imageObject.width;
  newImg.height = imageObject.height;
  createImage(newImg, imageObject.url)
    .then(function(img) {
      b64ImageDict[imageObject.id] = getB64FromImg(newImg);
    })
}

function imageFileToB64(src) {
  return new Promise(function(resolve) {
    var reader = new FileReader();
    reader.onload = resolve
    reader.readAsDataURL(src);
  })
}

function createImage(img, imageUrl) {
  return new Promise(function(resolve) {
    img.crossOrigin = 'anonymous';
    img.onload = function() {
      resolve(this)
    }
    img.src = imageUrl;
  });
}

function getContentType(url) {
  if (url.indexOf('image/png') !== -1) {
    return 'image/png';
  } else if (url.indexOf('image/jpeg') !== -1) {
    // return 'image/jpeg'; TODO: support jpeg
    return null;
  } else {
    return null;
  }
}

function loadImage(src) {
  return imageFileToB64(src)
    .then(function(e) {
      var b64Url = e.target.result;
      b64ImageDict['0'] = b64Url;
      var contentType = getContentType(b64Url);
      if (contentType === null) {
        return null;
      }
      var b64String = e.target.result.replace('data:' + contentType + ';base64,', '');
      var blob = b64toBlob(b64String, contentType);
      var imageUrl = DOMURL.createObjectURL(blob);
      var newImage = new Image();
      return createImage(newImage, imageUrl)
    }).then(function(img) {
      if (img === null) {
        return null;
      } else {
        return {
          id: '0',
          url: img.src,
          width: img.naturalWidth,
          height: img.naturalHeight,
          originalWidth: img.naturalWidth,
          originalHeight: img.naturalHeight
        }
      }
    });
}

function b64toBlob(b64Data, contentType) {
  var contentType = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : '';
  var sliceSize = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 512;

  var byteCharacters = atob(b64Data);
  var byteArrays = [];
  for (var offset = 0; offset < byteCharacters.length; offset += sliceSize) {
    var slice = byteCharacters.slice(offset, offset + sliceSize);
    var byteNumbers = new Array(slice.length);
    for (var i = 0; i < slice.length; i++) {
      byteNumbers[i] = slice.charCodeAt(i);
    }
    var byteArray = new Uint8Array(byteNumbers);
    byteArrays.push(byteArray);
  }
  return new Blob(byteArrays, { type: contentType });
}

function toImageObject(image) {
  var id = image.getAttribute('data-imageuploadid');
  var originalWidth = parseFloat(image.getAttribute('data-original-width'));
  var originalHeight = parseFloat(image.getAttribute('data-original-height'));
  var width = parseFloat(image.style.width.replace('px', ''));
  var height = width / (originalWidth / originalHeight);
  return {
    id: id,
    url: image.src,
    originalWidth: originalWidth,
    originalHeight: originalHeight,
    width: width,
    height: height
  };
}

function exportImage() {
  var svg = document.getElementById("drawing-area");
  var svgCopy = svg.cloneNode(true);
  var sanitizedSvg = encodeSvgBlobs(svgCopy);
  getPng(sanitizedSvg)
    .then(exportHelper);
}

function exportHelper(image) {
  if (isInZendeskAppContext) {
    exportToZendesk(image);
  } else {
    exportToWeb(image);
  }
}

function encodeSvgBlobs(svg) {
  var imageBlobUrl = svg.querySelector('image').getAttribute('xlink:href');

  var b64Url = b64ImageDict[zendeskExportId];
  var images = svg.querySelectorAll('image');
  images[0].setAttribute('xlink:href', b64Url);
  images[1].setAttribute('xlink:href', b64Url);
  return svg;
}

function autoScale(canvas) {
  // adapted from https://www.html5rocks.com/en/tutorials/canvas/hidpi/
  var context = canvas.getContext('2d');
  var devicePixelRatio = window.devicePixelRatio || 1;
  var backingStoreRatio = context.webkitBackingStorePixelRatio ||
                        context.mozBackingStorePixelRatio ||
                        context.msBackingStorePixelRatio ||
                        context.oBackingStorePixelRatio ||
                        context.backingStorePixelRatio || 1;
  var ratio = devicePixelRatio / backingStoreRatio;
  if (devicePixelRatio !== backingStoreRatio) {
      var oldWidth = canvas.width;
      var oldHeight = canvas.height;
      canvas.width = oldWidth * ratio;
      canvas.height = oldHeight * ratio;
      canvas.style.width = oldWidth + 'px';
      canvas.style.height = oldHeight + 'px';
      context.scale(ratio, ratio);
  }
}

function exportToZendesk(image) {
  client.get('ticket.comment').then(function (zaf) {
    var div = document.createElement('div');
    var comment = zaf['ticket.comment'].text;
    div.innerHTML = comment;
    var selectionBox = div.querySelector('span.zd-editor--rich-text-comment--resize-box');
    var imageToReplace = div.querySelector('img[data-imageuploadid="' + zendeskExportId + '"]');
    if (selectionBox && imageToReplace) {
      selectionBox.outerHTML = '';
    } else {
      imageToReplace.outerHTML = '';
    }
    client.set('ticket.comment.text', div.innerHTML).then(function () {
      client.invoke('ticket.editor.inlineImage', image.src);
      client.invoke('app.close');
      app.ports.reset.send(null);
    });
  });
}

function exportToWeb(image) {
  var a = document.createElement('a');
  a.href = image.src;
  a.download = "output.png";
  document.body.append(a);
  a.click();
  document.body.removeChild(a);
}

function getPng(svg) {
  var svgData = new XMLSerializer().serializeToString(svg);
  lastSvgString = svgData;
  var width = parseInt(svg.getAttribute('width'), 10);
  var height = parseInt(svg.getAttribute('height'), 10);

  var newImage = document.createElement('img');
  newImage.width = width;
  newImage.height = height;
  return createImage(newImage, "data:image/svg+xml;base64," + btoa(svgData))
    .then(function() {
      var imageUrl = getB64FromImg(newImage);
      newImage.src = imageUrl;
      return newImage;
    });
}

function getB64FromImg(img) {
  var canvas = document.createElement("canvas");
  canvas.width = img.width;
  canvas.height = img.height;
  autoScale(canvas);
  var ctx = canvas.getContext("2d");
  ctx.drawImage(img, 0, 0, img.width, img.height);
  return canvas.toDataURL('image/png');
}

function readInImagesFromZendesk() {
  client.get('ticket.comment').then(function (zaf) {
    var div = document.createElement('div');
    var comment = zaf['ticket.comment'].text;
    div.innerHTML = comment;
    var selectedImage = div.querySelector('span.zd-editor--rich-text-comment--resize-box img');
    if (selectedImage) {
      var imageObject = toImageObject(selectedImage);
      app.ports.newImage.send(imageObject);
      loadImageDataIntoCache(imageObject);
    } else {
      var images = div.querySelectorAll('img');
      var imageObjects = Array.prototype.map.call(images, toImageObject);
      app.ports.setImages.send(imageObjects);
      Array.prototype.forEach.call(imageObjects, loadImageDataIntoCache);
    }
  });
}


// polyfills
if (!HTMLCanvasElement.prototype.toBlob) {
 Object.defineProperty(HTMLCanvasElement.prototype, 'toBlob', {
  value: function (callback, type, quality) {

    var binStr = atob( this.toDataURL(type, quality).split(',')[1] ),
        len = binStr.length,
        arr = new Uint8Array(len);

    for (var i = 0; i < len; i++ ) {
     arr[i] = binStr.charCodeAt(i);
    }

    callback( new Blob( [arr], {type: type || 'image/png'} ) );
  }
 });
}
