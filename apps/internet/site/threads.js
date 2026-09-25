// about:threads' script: comments folded as Hacker News' hn.js folds
// them, in the same ES5 -- written for TinyChrome's engine to run.

function Thread(el) {
  this.el = el;
  this.indent = parseInt(el.getAttribute('data-indent'), 10);
}
Thread.prototype.folded = function () { return this.el.classList.contains('folded'); };
Thread.prototype.toggle = function () {
  var fold = !this.folded();
  var toggle = this.el.getElementsByClassName('togg')[0];
  if (fold) { this.el.classList.add('folded'); } else { this.el.classList.remove('folded'); }
  var n = 0;
  // its replies: the comments after it, deeper than it
  for (var next = this.el.nextElementSibling; next && new Thread(next).indent > this.indent; next = next.nextElementSibling) {
    if (fold) { next.classList.add('noshow'); } else { next.classList.remove('noshow'); }
    n++;
  }
  toggle.innerHTML = fold ? '[' + (n + 1) + ' more]' : '[-]';
};

// how many comments, and whose names: a regular expression on the heads
var heads = document.getElementsByClassName('head');
var names = [];
Array.prototype.forEach.call(heads, function (h) {
  var m = h.textContent.match(/^(\w+) (\d+) hours?/);
  if (m && names.indexOf(m[1]) < 0) names.push(m[1]);
});
document.getElementById('count').textContent = heads.length + ' comments, by ' + names.join(', ') + '.';

document.addEventListener('click', function (ev) {
  if (ev.target.className == 'togg') {
    new Thread(ev.target.parentNode.parentNode).toggle();
    ev.preventDefault();
  }
});
