In following the Rust developer's progress, I've noticed that one of
the more prolific programmers is Marijn Haverbeke. He's pretty
well-known in the JavaScript community for his book "Eloquent
JavaScript." I started tralling social media for him and found both
his Twitter feed and personal website. Of course, one of the things
that caught my eye was the page "Continuation-Passing Style, and why
JavaScript developers might be interested in it." I knew that others
at Mozilla were big Schemers, but this was my first indication that
Marijn was one of them.I started looking through Marijn's explanation
of CPS, and he present the following direct-style code:

function traverseDocument(node, func) {
  func(node);
  var children = node.childNodes;
  for (var i = 0; i < children.lenght; i++)
    traverseDocument(children[i], func);
}

function capitaliseText(node) {
  if (node.nodeType == 3) // a text node
    node.nodeValue = node.nodeValue.toUpperCase();
}

traverseDocument(document.body, capitaliseText);

Marijn then goes into how to transform it to CPS. He explains how each
function receives a new argument for the continuation, which he
understandably but irksomely calls 'c,' for the function representing
the work that needs to be done once the current function call
returns. He goes into some of the nice properties about CPS, including
the fact that it should make the stack irrelevant for calls to
functions in CPS. He then shows his re-write of the above code in CPS:

function traverseDocument(node, funct, c) {
  var children = node.childNodes;
  function handleChildren(i, c) {
    if (i < children.length)
      traverseDocument(children[i], func,
                       function(){handleChildren(i + 1, c);});
    else
      c();				 
  }
  return func(node, function(){handleChildren(0, c);});
}

function capitaliseText(node, c) {
  if (node.nodeType == 3)
    node.nodeValue = node.nodeValue.toUpperCase();
  c();
}

traverseDocument(document.body, capitaliseText, function(){});

