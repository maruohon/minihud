package fi.dy.masa.minihud.config.gui;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.IConfigValue;
import fi.dy.masa.malilib.config.gui.ConfigPanelBase;
import fi.dy.masa.malilib.config.gui.GuiModConfigs;
import fi.dy.masa.malilib.gui.ConfigInfoProviderSimple;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;

public class MiniHudConfigPanel extends ConfigPanelBase
{
    @Override
    protected String getPanelTitlePrefix()
    {
        return Reference.MOD_NAME + " options";
    }

    @Override
    protected void createSubPanels()
    {
        String modId = Reference.MOD_ID;
        List<? extends IConfigValue> configs;
        ConfigInfoProviderSimple provider;

        this.addSubPanel(new GuiModConfigs(modId, "Generic", Configs.Generic.OPTIONS));
        this.addSubPanel((new GuiModConfigs(modId, "Colors", Configs.Colors.OPTIONS)).setConfigWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new GuiModConfigs(modId, "Info Toggles", configs)).setConfigWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new GuiModConfigs(modId, "Info Line Order", configs)).setConfigWidth(60));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values()));
        provider = new ConfigInfoProviderSimple("Hotkey to toggle the '", "' info line");
        this.addSubPanel((new GuiModConfigs(modId, "Info Hotkeys", configs)).setHoverInfoProvider(provider));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values()));
        provider = new ConfigInfoProviderSimple("Hotkey to toggle the '", "' renderer");
        this.addSubPanel((new GuiModConfigs(modId, "Renderer Hotkeys", configs)).setHoverInfoProvider(provider));
    }
}
