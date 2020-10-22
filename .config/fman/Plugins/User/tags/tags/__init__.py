from fman import ApplicationCommand, DirectoryPaneCommand, show_alert, show_prompt, show_quicksearch, QuicksearchItem

# https://fman.io/docs/api
class Tag(DirectoryPaneCommand):
    def __call__(self):
        chosen_files = self.get_chosen_files()
        if not chosen_files:
            show_alert('No files are selected!')
            return
        else:
            text, ok = show_prompt('Please enter a tag name')
            if text and ok:
                show_alert(f'Tagging `{chosen_files}` as `{text}`')
            elif not text and ok:
                result = show_quicksearch(self._get_items)
                if result:
                    query, value = result
                    show_alert('You typed %r and selected %r.' % (query, value))
                else:
                    show_alert('You cancelled the dialog.')

    def _get_items(self, query):
        for item in ['Some item', 'Another item', 'And another']:
            try:
                index = item.lower().index(query)
            except ValueError as not_found:
                continue
            else:
                # The characters that should be highlighted:
                highlight = range(index, index + len(query))
                yield QuicksearchItem(item, highlight=highlight)

