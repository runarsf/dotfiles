from fman import ApplicationCommand, DirectoryPaneCommand, show_alert, show_prompt, show_quicksearch, QuicksearchItem

# https://fman.io/docs/api

# Check if schema is file
# DirectoryPaneListener.on_command(command_name, args)

class Tag(DirectoryPaneCommand):
    def __call__(self):
        chosen_files = self.get_chosen_files()
        if not chosen_files:
            show_alert('No files are selected!')
            return
        else:
            result = show_quicksearch(self._get_items)
            if result:
                query, value = result
                #show_alert('You typed %r and selected %r.' % (query, value))
                if value == 'New Tag':
                    text, ok = show_prompt('Please enter a new tag name')
                    if text and ok:
                        show_alert(f'Creating new tag `{text}`')
                        value = text
                    else:
                        show_alert('No tag entered, aborting...')
                        return
                show_alert(f'Tagging `{chosen_files}` as `{value}`')
            else:
                show_alert('You cancelled the dialog.')
                return

    def _get_items(self, query):
        for item in ['Some item', 'Another item', 'And another', 'New Tag']:
            try:
                index = item.lower().index(query)
            except ValueError as not_found:
                continue
            else:
                # The characters that should be highlighted:
                highlight = range(index, index + len(query))
                yield QuicksearchItem(item, highlight=highlight)

