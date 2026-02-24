# Specification

## Summary
**Goal:** Add GIF sharing capability to the chat feature by allowing users to paste direct .gif URLs.

**Planned changes:**
- Add support for pasting direct .gif URLs (from media.tenor.com, c.tenor.com, or any other direct .gif link) into the chat input
- Validate that URLs end in .gif and provide feedback for invalid URLs
- Render GIF URLs as animated images inline in chat messages
- Update backend Message type to store and retrieve GIF URLs
- Maintain GIF animation and ensure proper sizing within the chat layout

**User-visible outcome:** Users can share animated GIFs in chat by pasting direct .gif URLs, which will display as animated images in the conversation alongside text, images, stickers, and dice rolls.
