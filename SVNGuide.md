# SVN installation guide #
  1. Install any SVN manager. I will describe this from TortoiseSVN point of view, which is freely available for Windows.
  1. Make new folder (for example name it "nx"), and right click it to open menu
    * Select "SVN Checkout..."
      * Set URL to: http://nxpascal.googlecode.com/svn/trunk/
      * Make sure these are selected: "Fully recursive", "HEAD revision"
      * Click OK
  1. Congratulations, you are all set!

You can now go into demos\ folders, to try and learn from different projects.

For your game projects, it is recommended to copy the "Game template" demo to your new project folder.

All demos should work as is, but for your own projects you need to set nx\src\ path to your programming IDE. If you use Delphi, this is in Environment path, saved globally for every project. For Lazarus you need to do this for every individual project:
Project menu -> Project Options -> Paths -> Other Unit files...


---

## Synapse ##
SVN trunk for Synapse network library is at:
https://synalist.svn.sourceforge.net/svnroot/synalist/trunk/

Check here if it's changed: http://www.ararat.cz/synapse/doku.php/download