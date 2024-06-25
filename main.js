import './style.css'
import blank from './public/blank.png'
import down from './public/down.png'
import { Elm } from './src/Main.elm'


// Mount "Hello" Browser.{element,document} on #root
Elm.Main.init({
  node: document.getElementById('app'),
  flags: { blank: blank, down: down} 
})
