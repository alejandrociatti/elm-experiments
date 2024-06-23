import './style.css'
import blank from './public/blank.png'
import up from './public/up.png'
import down from './public/down.png'
import left from './public/left.png'
import right from './public/right.png'
import { Elm } from './src/Main.elm'


// Mount "Hello" Browser.{element,document} on #root
Elm.Main.init({
  node: document.getElementById('app'),
  flags: { blank: blank, up: up, down: down, left: left, right: right} 
})
