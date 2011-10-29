package ideah.tree;

import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellTokenTypes;

import java.util.Collections;

public final class Filler extends Located {

    public final IElementType type;
    public final String text;

    public Filler(IRange location, IElementType type, String text) {
        super(location);
        this.type = type;
        this.text = text;
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }

    @Override
    public String toString() {
        return type + " " + text + " " + location;
    }

    @Override
    protected boolean isKeyword(String keyword) {
        return type == HaskellTokenTypes.KEYWORD && keyword.equals(text);
    }
}
