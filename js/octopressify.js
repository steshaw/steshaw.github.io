'use strict';$(function(){$('.sourceCode[data-title]').each(function(){var el=$(this),title=el.data('title'),link=el.data('link'),linkTitle=el.data('link-title');if(linkTitle===undefined)linkTitle='Link';var figure=document.createElement('figure'),classAttribute=document.createAttribute('class');classAttribute.value='code';figure.setAttributeNode(classAttribute);var figcaption=document.createElement('figcaption'),titleText=document.createTextNode(title);figcaption.appendChild(titleText);if(link!==undefined){var link=$.parseHTML('<a href="'+link+'">'+linkTitle+'</a>');$(figcaption).append(link)};figure.appendChild(figcaption);figure.appendChild(el[0].cloneNode(true));var element=el[0];element.parentNode.replaceChild(figure,element)})})