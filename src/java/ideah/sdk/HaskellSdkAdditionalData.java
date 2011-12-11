package ideah.sdk;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModel;
import ideah.util.GHCUtil;
import org.jdom.Element;

import java.io.File;

public final class HaskellSdkAdditionalData implements SdkAdditionalData {

    private static final String LIB_PATH = "ghcLibPath";
    private static final String CABAL_PATH = "cabalPath";
    private static final String GHC_OPTIONS = "ghcOptions";

    private String libPath;
    private String cabalPath;
    private String ghcOptions;
    private final HaskellSdkConfigurable haskellSdkConfigurable;

    public HaskellSdkAdditionalData(String libPath, String cabalPath, String ghcOptions, HaskellSdkConfigurable haskellSdkConfigurable) {
        this.libPath = libPath;
        this.cabalPath = cabalPath;
        this.ghcOptions = ghcOptions;
        this.haskellSdkConfigurable = haskellSdkConfigurable;
    }

    public HaskellSdkAdditionalData(Element element) {
        this.libPath = element.getAttributeValue(LIB_PATH);
        this.cabalPath = element.getAttributeValue(CABAL_PATH);
        this.ghcOptions = element.getAttributeValue(GHC_OPTIONS);
        this.haskellSdkConfigurable = null;
    }

    public void checkValid(SdkModel sdkModel) throws ConfigurationException {
        for (Sdk sdk : sdkModel.getSdks()) {
            // todo: must check this.properties?
            SdkAdditionalData sdkAdditionalData = sdk.getSdkAdditionalData();
            if (sdkAdditionalData instanceof HaskellSdkAdditionalData) {
                HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) sdkAdditionalData;
                // todo: changed in GHC 7?
                if (!new File(data.getLibPath(), "package.conf.d").isDirectory()) {
                    haskellSdkConfigurable.reset(); // todo: ignored?! (produce error message analogous to ghc home)
                    throw new ConfigurationException("Invalid GHC lib directory (should contain 'package.conf.d' folder)");
                }
                String cabal = GHCUtil.getExeName("cabal");
                if (!new File(data.getCabalPath()).getName().equals(cabal)) { // todo: WTF???
                    haskellSdkConfigurable.reset(); // todo
                    throw new ConfigurationException("Please indicate the full " + cabal + " file path");
                }
            }
        }
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    public void save(Element element) {
        if (libPath != null) {
            element.setAttribute(LIB_PATH, libPath);
        }
        if (cabalPath != null) {
            element.setAttribute(CABAL_PATH, cabalPath);
        }
        if (ghcOptions != null) {
            element.setAttribute(GHC_OPTIONS, ghcOptions);
        }
    }

    public String getLibPath() {
        return libPath;
    }

    public String getCabalPath() {
        return cabalPath;
    }

    public String getGhcOptions() {
        return ghcOptions;
    }

    public void setLibPath(String libPath) {
        this.libPath = libPath;
    }

    public void setCabalPath(String cabalPath) {
        this.cabalPath = cabalPath;
    }

    public void setGhcOptions(String ghcOptions) {
        this.ghcOptions = ghcOptions;
    }
}
