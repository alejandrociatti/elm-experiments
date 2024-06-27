import blank from '../../../assets/blank.png'
import down from '../../../assets/down.png'
import { Elm } from '../../../elm/WFC/Svg.elm'


// Mount "Hello" Browser.{element,document} on #root
Elm.WFC.Svg.init({
  node: document.getElementById('wave-function-collapse'),
  flags: { blank: blank, down: down} 
})
