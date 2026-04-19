---
name: session-end
description: End-of-session workflow for Ventura project. Creates session notes, updates CHANGELOG.md, commits, tags, and pushes.
disable-model-invocation: true
---

# Session End Workflow

When the user indicates the session is ending (says "update CLAUDE.md", "end of session", "wrap up", etc.), follow these steps in order:

## 1. Create Session Notes

Create a new file at `Session_Notes/YYYY-MM-DD_brief_description.md` (today's date, description summarizing the session theme).

Include these sections:
- **Summary of Changes**: Bulleted list of what was done
- **Testing Checklist**: Markdown checkboxes for manual verification
- **Next Steps**: Follow-up tasks

## 2. Update CHANGELOG.md

Add new dated entries at the top of CHANGELOG.md (below the header) for each completed item:

```markdown
## YYYY-MM-DD
- **Brief title**: One-line description
```

## 3. Update CLAUDE.md (only if needed)

Only modify CLAUDE.md if:
- A priority was completed or a new one identified
- A remaining issue was resolved or discovered
- Infrastructure details changed

Do NOT add completed items to CLAUDE.md — those go in CHANGELOG.md only.

## 4. Determine Git Tag Version

```bash
git tag --sort=-v:refname | head -1
```

Increment minor version (v1.19 -> v1.20). Major version only for breaking changes.

## 5. Commit, Tag, Push

```bash
cd /home/fls/Models/Ventura/HD
git add -A
git commit -m "Description of session changes"
git tag vX.Y-brief-description
git push && git push --tags
```

Tag format: `vX.Y-brief-description` with hyphens (e.g., `v1.20-claude-md-reorganization`).
