import "elm-canvas"
import './style.css'
import blank from './assets/blank.png'
import down from './assets/down.png'
import { Elm } from './elm/Main.elm'


// Mount "Hello" Browser.{element,document} on #root
Elm.Main.init({
  node: document.getElementById('app'),
  flags: { blank: blank, down: down} 
})
