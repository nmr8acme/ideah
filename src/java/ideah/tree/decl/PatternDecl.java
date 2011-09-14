package ideah.tree.decl;

import ideah.tree.GRHSs;
import ideah.tree.pat.Pat;

public final class PatternDecl extends Declaration {

    public final Pat pattern;
    public final GRHSs rightHand;

    public PatternDecl(Pat pattern, GRHSs rightHand) {
        this.pattern = pattern;
        this.rightHand = rightHand;
    }
}
