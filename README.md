# NEMACS - Emacs Documentation

## Philosophy

It's been more than 2 years since I started to use Emacs for everything. My first commit was on May 2017 and surprisingly I was on MacOS. Although that wasn't my first time using Emacs, as I had use it for work weeks before that on Ubuntu, but it was what pulled me over to the Emacs world as I really hated the Mac and all the environment and Emacs helped me use it a little bit more.

From there I never looked at any other editor, or operating system for that matter. Emacs had it all and it also created some fun moments at work when my friends/co-workers gave me different "challenges" or things to do with Emacs. One of my best friends kind of played with me, but he was using Vim so I had to show him what Emacs could do. First, I showed him Twitter running on Emacs, then I controlled my Spotify playlist with the keyboard and while coding, and after that I had my work chat through ERC and Bitlbee, that was fun.

That same year a lot of things in my life changed. I moved to another Company, started traveling a lot more and learned the ways of Getting Things Done by David Allen, I can say it's one of my favorite books still Today. The only thing that didn't change was Emacs, I still used Emacs for everything. Now I had my Calendar, my Daily Reviews, JIRA tickets and stories, tracking my time, e-mail, chat and obviously, coding. The only problem I found with Emacs is that my configuration and code was getting out of hand, and I couldn't find a way to make it neat while working a lot.

Then 2019 came over and my life is changing again, through the same period as the 2017 changes. Now this inspired me a lot, because I went from a 3 to 4 months hiatus into work again and now my work is not about coding but documenting and analyzing technical solutions. So I thought I have to ride this wave of inspiration and change my Emacs too (but no, not leaving Emacs). Now that I've learned a lot throughout these two years and I have the time to sit down and start all over, I can make this configuration neat, organized and scalable as I always wanted.

So here it's, NEMACS (or Nahuel-EMACS) version 2.0 (actually 5, but yeah) with a clean architecture and neat documentation.

-- Nahuel Jesús Sacchetti https://nsacchetti.com

## Architecture

NEMACS follows the principle of "one function does only one thing" (basic functional programming here). Also, "one file does one thing" can be applied. The idea of separating the logic by module is something that I had in my second version of NEMACS, where the code was written inside an _ORG_ file and it was separated by what the code did in the grand scheme of things.

Now NEMACS has its basics separated in different files, `init.el` loads the whole thing in a "lifecycle" fashion and after everything (Emacs core-related) is initialized, the modules are loaded one by one.

| Name            | Type   | Description                                                                                                       |
|-----------------|--------|-------------------------------------------------------------------------------------------------------------------|
| ~/.emacs.d      | Folder | The main Emacs configuration folder. Also saved in variable `nemacs-emacs-dir`                                    |
| .local          | Folder | Local folder, not shared upstream. Contains all the system's folders like `cache`, `etc` and `packages`           |
| .local/cache    | Folder | Saved configurations and navigations that can be safely deleted                                                   |
| .local/etc      | Folder | Persisted configuration is saved here. Like `bookmarks`                                                           |
| .local/packages | Folder | Packages downloaded from `MELPA` live here                                                                        |
| modules         | Folder | Modules files live here. The name of the files inside should be `<modulename>.module.el`                          |
| elisp           | Folder | Custom functions and packages that I create are here                                                              |
| themes          | Folder | Contains all the themes I use or used in Emacs                                                                    |
| keybindings.el  | File   | File containing configurations for my custom keybindings                                                          |
| hooks.el        | File   | Contains all the hook bindings                                                                                    |
| init.el         | File   | Initialization file                                                                                               |
| packages.el     | File   | Initialize the Package Manager and downloads the necessary packages                                               |
| preload.el      | File   | Runs before the `after-init-hook`. Contains defaults and configurations that are needed before Emacs ends loading |
| theme.el        | File   | Custom theme changes for the current selected theme are declared here                                             |
| vars.el         | File   | Initialize the custom variables needed for the configuration                                                      |

### The `NEMACS Lifecycle`

Any Emacs configuration is inherently complicated and coupled. Things need to be loaded in specific timings that makes sense, user can access to `hooks` to delay running a function until this other action happens (and sometimes there are multiple `hooks` for one function running), there are also multiple `eval` functions (when compiling, after loading, and more). So in order to have a clean configuration, first problem you need to solve is how we load everything. I thought following a `lifecycle` model is the best way to do it.

The `NEMACS Lifecycle` consists in:

- Initializing variables,
- Preloading necessary configuration,
- Loading the external packages,
- Setting the keybindings,
- Load the theme,
- Lastly, execute the `after-init-hook`.

### After the Initialization

#### Further controlling Emacs

Even though we have `hooks` and different ways to `eval` a function, surprisingly there are some functions in Emacs that have much less control, for example `keyboard-quit`. Some packages are solving this problem by adding a custom `hook`, like `Helm` does with `helm-quit-hook` or even `RMail` (which comes with Emacs). Here's where things like `nemacs-escape` comes into action. `nemacs-escape` replaces `keyboard-quit` and runs `nemacs-escape-hook` functions when doing it. Also, quit things progressively so instead of pressing `C-g` multiple times, one time will quit every action.

Another example is `C-a` or `move-beginning-of-line`. An essential problem with this function is that when used on indented content it will move the cursor to column 0. The problem is you will need to either press `TAB` or find the indentation yourself (depending on the `major-mode` active at the moment). `nemacs-move-beginning-of-line` fixes this issue by letting you press `C-a` to move to either column 0 or the indentation column (interchangeably, depending on the cursor position). So if you're currently in column 0, pressing `C-a` will move you to the indented column, but if you're in the indented column you can press `C-a` to go to column 0.

NEMACS follows these ideas for every other action where Emacs provides less control, improving functionality and customization.

#### NEMACS as the Operating System

One problem Emacs has when used "for everything" in your life is opening big files like images or huge PDF files. NEMACS also takes care of that through `nemacs-check-large-file`. This function is hooked into `find-file-hook` in order to check the size of the opened/selected file and, if the file exceeds the `nemacs-large-file-size`, ask the user what to do. The prompt will ask the user if they want to open this file `literally`, this means:

- The file will be opened in read-mode,
- The undo functionality in Emacs will be disabled for this file,
- The `major-mode` will be `fundamental-mode`.

This helps to not block the Emacs `runtime` while opening big files and solving this crucial issue.

#### NEMACS as the Window Manager

Emacs has almost all the facilities to be an Operating System. If we provide a Linux kernel we can make it our Window Manager. Since I already do all of my things in Emacs, except for browsing, I can transform NEMACS in my Window Manager. I do this with [EXWM](https://github.com/ch11ng/exwm).

`EXWM` supports multi-monitor, multiple workspaces, a system tray bar (although I use an external bar too in Polybar) and running X applications inside Emacs, while passing in any keybindings I want (like making Firefox scroll down when I press `C-n`).

### Modules
