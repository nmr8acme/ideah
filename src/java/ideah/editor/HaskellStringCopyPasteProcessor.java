package ideah.editor;

import com.intellij.codeInsight.editorActions.CopyPastePreProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.RawText;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.LineTokenizer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellTokenTypes;

public final class HaskellStringCopyPasteProcessor implements CopyPastePreProcessor {

    public String preprocessOnCopy(PsiFile file, int[] startOffsets, int[] endOffsets, String text) {
        boolean isLiteral = true;
        for (int i = 0; i < startOffsets.length; i++) {
            if (findLiteralTokenType(file, startOffsets[i], endOffsets[i]) == null) {
                isLiteral = false;
                break;
            }
        }
        // todo: use Haskell unescaping
        return isLiteral ? StringUtil.unescapeStringCharacters(text) : null;
    }

    public String preprocessOnPaste(Project project, PsiFile file, Editor editor, String text, RawText rawText) {
        Document document = editor.getDocument();
        PsiDocumentManager.getInstance(project).commitDocument(document);
        SelectionModel selectionModel = editor.getSelectionModel();

        // pastes in block selection mode (column mode) are not handled by a CopyPasteProcessor
        int selectionStart = selectionModel.getSelectionStart();
        int selectionEnd = selectionModel.getSelectionEnd();
        IElementType tokenType = findLiteralTokenType(file, selectionStart, selectionEnd);

        if (tokenType == HaskellTokenTypes.STRING) {
            if (rawText != null && rawText.rawText != null)
                return rawText.rawText; // Copied from the string literal. Copy as is.

            StringBuilder buffer = new StringBuilder(text.length());
            String breaker = "\\n\" ++\n\""; // todo: do indenting
            String[] lines = LineTokenizer.tokenize(text.toCharArray(), false, true);
            for (int i = 0; i < lines.length; i++) {
                if (i > 0) {
                    buffer.append(breaker);
                }
                String line = lines[i];
                buffer.append(StringUtil.escapeStringCharacters(line)); // todo: use Haskell escaping
            }
            text = buffer.toString();
        }
        return text;
    }

    private static IElementType findLiteralTokenType(PsiFile file, int selectionStart, int selectionEnd) {
        PsiElement elementAtSelection = file.findElementAt(selectionStart);
        if (elementAtSelection == null)
            return null;
        IElementType tokenType = elementAtSelection.getNode().getElementType();
        if (tokenType != HaskellTokenTypes.STRING)
            return null;
        TextRange textRange = elementAtSelection.getTextRange();
        if (selectionStart <= textRange.getStartOffset() || selectionEnd >= textRange.getEndOffset()) {
            return null;
        }
        return tokenType;
    }
}
