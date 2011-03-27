# Craig Andera's emacs Setup

This repository contains my personal emacs setup. It is intended to be used with emacs 23 - previous versions may not work at all. 

# Getting Started

This setup makes heavy use of git submodules, so there are a few extra steps to take. 

1. Clone this repository. 
1. Run <code>git submodule init</code>
1. Run <code>git submodule update</code>
1. Run <code>ln -s /path/to/this/repo ~/.emacs.d/custom</code>
1. Save anything from your ~/.emacs and ~/.emacs.d/init.el that you want to keep, because we're going to overwrite those files
1. Run <code>echo (load "~/.emacs.d/custom/init.el") > ~/.emacs.d/init.el</code>
1. Run <code>rm -f ~/.emacs</code>

That should do it!
