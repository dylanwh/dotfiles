# Updates Terminal Data widget in Secure ShellFish
#
# This command sends encrypted data through push notifications such that
# it doesn't need to run from a Secure ShellFish terminal.
#
# put this file inside $HOME/.config/fish/functions

function widget
  if not set -q argv[1]
    echo 'Usage: widget [target] <data> ...'
    echo
    echo 'Update widget on device from which this function was installed with a number of content parameters that can be string, progress, icon, target, color, url or shortcut.'
    echo
    echo 'Each argument type is derived from input, where the first argument is assumed to be a target if it matches a target configured on the widget.'
    echo
    echo 'Progress has the form: 50% or 110/220'
    echo
    echo 'Icon must match valid SF Symbol name such as globe or terminal.fill'
    echo
    echo 'Colors must be hex colours such as #000 #ff00ff where the color is used for later content and 'foreground' switches back to default colour'
    echo
    echo 'Target is used to send different content to different widgets after configuring the widgets with different target identifiers which requires the pro unlock. The target parameter is never assumed unless --target is used and is effective until next --target parameter allowing updates of several widgets with a single command'
    echo ''
    echo 'URL is used when tapping the widget and is assumed for arguments starting with https:// and other schemes are supported by using --url'
    echo
    echo 'Shortcut works like URL running the Shortcut with the given name and is never assumed without --shortcut'
    echo
    echo 'String is the fallback type if nothing else matches, but content type can be forced for next parameter with --progress, --icon, --color, --text or --target with something like:'
    echo '  widget --text "50/100"'
    echo
    echo 'You can update several widgets at once by using --target to send all parameters until the next --target to a particular widget. Updating several widgets at once allows more total updates per day.'
  else
    set --local key 3163253ebe341529c502ac907c03527f897db628d3ca99ed3a80cd69a073d468
    set --local user NibIpbMZ4i1aftgZTDF8SvETfpQGRWM5XXrnjXCn
    set --local iv ab5bbeb426015da7eedcee8bee3dffb7

    set plain "$(
      echo Secure ShellFish Widget 1.0
      for arg in $argv
        echo "$arg"
      end
    )"
    set --local base64 $(echo "$plain" | openssl enc -aes-256-cbc -base64 -K $key -iv $iv)
    curl -sS -X POST -H "Content-Type: text/plain" --data "$base64" "https://secureshellfish.app/push/?user=$user"

  end
end
