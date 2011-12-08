package ideah.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModificator;
import ideah.util.CompilerLocation;

import javax.swing.*;

public final class HaskellSdkConfigurable implements AdditionalDataConfigurable {

    private final HaskellSdkConfigurableForm myForm;

    private Sdk mySdk;

    public HaskellSdkConfigurable() {
        myForm = new HaskellSdkConfigurableForm();
    }

    public void setSdk(Sdk sdk) {
        mySdk = sdk;
        SdkAdditionalData sdkAdditionalData = sdk.getSdkAdditionalData();
        if (sdkAdditionalData instanceof HaskellSdkAdditionalData) {
            HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) sdkAdditionalData;
            if (data.getCabalPath() == null) {
                String cabalPath = CompilerLocation.suggestCabalPath(sdk);
                data.setCabalPath(cabalPath == null ? "" : cabalPath);
            }
            if (data.getGhcOptions() == null)
                data.setGhcOptions("");
            if (data.getLibPath() == null) {
                String libPath = CompilerLocation.suggestLibPath(sdk);
                data.setLibPath(libPath == null ? "" : libPath);
            }
        }
    }

    public JComponent createComponent() {
        return myForm.getContentPanel();
    }

    public boolean isModified() {
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) mySdk.getSdkAdditionalData();
        if (data == null)
            return true;
        return !(data.getCabalPath().equals(myForm.getCabalPath())
            && data.getGhcOptions().equals(myForm.getGhcOptions())
            && data.getLibPath().equals(myForm.getLibPath()));
    }

    public void apply() {
        String libPath = myForm.getLibPath();
        String cabalPath = myForm.getCabalPath();
        String ghcOptions = myForm.getGhcOptions();
        HaskellSdkAdditionalData newData = new HaskellSdkAdditionalData(libPath, cabalPath, ghcOptions, this);
        final SdkModificator modificator = mySdk.getSdkModificator();
        modificator.setSdkAdditionalData(newData);
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                modificator.commitChanges();
            }
        });
    }

    public void reset() {
        SdkAdditionalData data = mySdk.getSdkAdditionalData();
        if (!(data instanceof HaskellSdkAdditionalData)) {
            return;
        }
        HaskellSdkAdditionalData ghcData = (HaskellSdkAdditionalData) data;
        myForm.init(ghcData.getLibPath(), ghcData.getCabalPath(), ghcData.getGhcOptions());
    }

    public void disposeUIResources() {
    }
}
