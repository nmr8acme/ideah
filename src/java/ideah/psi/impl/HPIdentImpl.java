package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import ideah.psi.api.HPIdent;
import org.jetbrains.annotations.NotNull;

public final class HPIdentImpl extends HaskellAbstractIdentImpl  implements HPIdent {

    public HPIdentImpl(@NotNull ASTNode node) {
        super(node);
    }
}
