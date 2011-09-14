package ideah.tree.pat;

public final class BangPat extends Pat {

    public final Pat pattern;

    public BangPat(Pat pattern) {
        this.pattern = pattern;
    }
}
