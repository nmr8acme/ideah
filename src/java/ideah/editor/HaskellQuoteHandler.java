package ideah.editor;

import com.intellij.codeInsight.editorActions.SimpleTokenSetQuoteHandler;
import ideah.lexer.HaskellTokenTypes;

public final class HaskellQuoteHandler extends SimpleTokenSetQuoteHandler {

    public HaskellQuoteHandler() {
        super(HaskellTokenTypes.STRINGS);
    }
}
