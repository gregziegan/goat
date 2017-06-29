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
    if (!file && !file.type.match(/image.*/)) {
      return;
    }
    loadImage(file)
      .then(function(loadedImage) {
        app.ports.newImage.send(loadedImage);
      })
  }, true);
});

app.ports.requestImages.subscribe(function () {
  if (isInZendeskAppContext) {
    readInImagesFromZendesk();
  } else {
    var goatUrls = [ GOAT_PATH + 'goat.jpg', GOAT_PATH + 'goat2.jpg' ];
    Promise.all(Array.prototype.map.call(goatUrls, function (url) {
      return createImage(new Image(), url);
    })).then(function (goatImages) {
       var goats = defaultGoats(goatImages);
       app.ports.setImages.send(goats);
       Array.prototype.forEach.call(goats, loadImageDataIntoCache);
    })
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
function defaultGoats(goatImages) {
  var goats =
    [{ id: '0', url: null, width: 235, height: 276, originalWidth: 639, originalHeight: 751},
     { id: '1', url: null, width: 294, height: 220, originalWidth: 800, originalHeight: 600}]
   Array.prototype.forEach.call(goatImages, function(goatImage, index) {
     goats[index].url = getB64FromImg(goatImage);
   })
  return goats;
}

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

function loadImage(src) {
  return imageFileToB64(src)
    .then(function(e) {
      var b64Url = e.target.result;
      b64ImageDict['0'] = b64Url;
      return createImage(new Image(), b64Url)
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
  var sanitizedSvg = b64EncodeSvgImages(svgCopy);
  getImageUrlForSvg(sanitizedSvg)
    .then(exportToPlatform);
}

function b64EncodeSvgImages(svg) {
  var imageBlobUrl = svg.querySelector('image').getAttribute('xlink:href');

  var b64Url = b64ImageDict[zendeskExportId];
  var images = svg.querySelectorAll('image');
  images[0].setAttribute('xlink:href', b64Url);
  images[1].setAttribute('xlink:href', b64Url);
  return svg;
}

function exportToPlatform(imageUrl) {
  if (isInZendeskAppContext) {
    exportToZendesk(imageUrl);
  } else {
    exportToWeb(imageUrl);
  }
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

function exportToZendesk(imageUrl) {
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
      client.invoke('ticket.editor.inlineImage', imageUrl);
      client.invoke('app.close');
      app.ports.reset.send(null);
    });
  });
}

function exportToWeb(imageUrl) {
  var a = document.createElement('a');
  a.href = imageUrl;
  a.download = "output.png";
  document.body.append(a);
  a.click();
  document.body.removeChild(a);
}

function getImageUrlForSvg(svg) {
  var svgData = new XMLSerializer().serializeToString(svg);
  var width = parseInt(svg.getAttribute('width'), 10);
  var height = parseInt(svg.getAttribute('height'), 10);

  var newImage = new Image();
  newImage.width = width;
  newImage.height = height;
  return createImage(newImage, "data:image/svg+xml;base64," + btoa(svgData))
    .then(function() {
      return getB64FromImg(newImage);
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
