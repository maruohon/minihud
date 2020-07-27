package fi.dy.masa.minihud.config.gui;

import java.util.ArrayList;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.gui.config.GuiModConfigs;
import fi.dy.masa.malilib.gui.config.liteloader.ConfigPanelBase;
import fi.dy.masa.malilib.config.option.IConfigBase;
import fi.dy.masa.malilib.config.option.IConfigValue;
import fi.dy.masa.malilib.gui.util.ConfigInfoProviderSimple;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

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

        this.addSubPanel(new GuiModConfigs(modId, Configs.Generic.OPTIONS, "minihud.gui.button.config_gui.generic"));
        this.addSubPanel((new GuiModConfigs(modId, Configs.Colors.OPTIONS, "minihud.gui.button.config_gui.colors")).setConfigWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new GuiModConfigs(modId, configs, "minihud.gui.button.config_gui.info_toggles")).setConfigWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new GuiModConfigs(modId, configs, "minihud.gui.button.config_gui.info_line_order")).setConfigWidth(60));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values()));
        provider = new ConfigInfoProviderSimple("Hotkey to toggle the '", "' info line");
        this.addSubPanel((new GuiModConfigs(modId, configs, "minihud.gui.button.config_gui.info_hotkeys")).setHoverInfoProvider(provider));

        List<IConfigBase> list = new ArrayList<>();
        list.addAll(StructureToggle.getToggleConfigs());
        list.addAll(StructureToggle.getHotkeys());
        list.addAll(StructureToggle.getColorConfigs());
        this.addSubPanel((new GuiModConfigs(modId, list, "minihud.gui.button.config_gui.structures")).setConfigWidth(200));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values()));
        provider = new ConfigInfoProviderSimple("Hotkey to toggle the '", "' renderer");
        this.addSubPanel((new GuiModConfigs(modId, configs, "minihud.gui.button.config_gui.renderer_hotkeys")).setHoverInfoProvider(provider));
    }
}
