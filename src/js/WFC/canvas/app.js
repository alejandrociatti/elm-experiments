import "elm-canvas"
import '../../../style.css'
import blank from '../../../assets/blank.png'
import down from '../../../assets/down.png'
import { Elm } from '../../../elm/Experiments/WFC/Canvas.elm'

function loadImage(src) {
  return new Promise((resolve, reject) => {
    let image = new Image();
    image.crossOrigin = "Anonymous";
    image.onload = () => resolve(image);
    image.onerror = (err) => reject(err);
    image.src = src;
  });
}

let imageUrls = [
  blank,
  down
];

Promise.all(imageUrls.map(loadImage))
  .then((images) => {
    let app = Elm.Experiments.WFC.Canvas.init({
      node: document.getElementById('wave-function-collapse'),
      flags: images
    })
  })
  .catch((err) =>
    alert(
      // "Failed to load images\n\n" + err.path.map((i) => i.src).join("\n")
      "Failed to load images\n\n" + err
    )
  );