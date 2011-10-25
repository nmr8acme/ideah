package ideah.findUsages;

import com.intellij.openapi.application.QueryExecutorBase;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import ideah.psi.impl.HPIdentImpl;
import org.jetbrains.annotations.NotNull;

public final class HaskellReferencesSearch extends QueryExecutorBase<PsiReference, ReferencesSearch.SearchParameters> {

    public HaskellReferencesSearch() {
        super(true);
    }

    @Override
    public void processQuery(@NotNull ReferencesSearch.SearchParameters queryParameters, @NotNull Processor<PsiReference> consumer) {
        PsiElement element = queryParameters.getElementToSearch();
        if (element instanceof HPIdentImpl) {
            HPIdentImpl ident = (HPIdentImpl) element;
            consumer.process(ident);
            //GroovyConstructorUsagesSearcher.processConstructorUsages(ident, queryParameters.getScope(), consumer, queryParameters.getOptimizer(), true, false);
        }
    }
}
