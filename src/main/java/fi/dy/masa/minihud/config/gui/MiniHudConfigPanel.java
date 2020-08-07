package fi.dy.masa.minihud.config.gui;

import java.util.ArrayList;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.gui.config.ModConfigScreen;
import fi.dy.masa.malilib.gui.config.liteloader.BaseConfigPanel;
import fi.dy.masa.malilib.config.option.ConfigOption;
import fi.dy.masa.malilib.config.option.IConfigValue;
import fi.dy.masa.malilib.gui.config.SimpleConfigInfoProvider;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class MiniHudConfigPanel extends BaseConfigPanel
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
        SimpleConfigInfoProvider provider;

        this.addSubPanel(new ModConfigScreen(modId, Configs.Generic.OPTIONS, "minihud.gui.button.config_gui.generic"));
        this.addSubPanel((new ModConfigScreen(modId, Configs.Colors.OPTIONS, "minihud.gui.button.config_gui.colors")).setConfigElementsWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new ModConfigScreen(modId, configs, "minihud.gui.button.config_gui.info_toggles")).setConfigElementsWidth(100));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values()));
        this.addSubPanel((new ModConfigScreen(modId, configs, "minihud.gui.button.config_gui.info_line_order")).setConfigElementsWidth(60));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values()));
        provider = new SimpleConfigInfoProvider("Hotkey to toggle the '", "' info line");
        this.addSubPanel((new ModConfigScreen(modId, configs, "minihud.gui.button.config_gui.info_hotkeys")).setHoverInfoProvider(provider));

        List<ConfigOption> list = new ArrayList<>();
        list.addAll(StructureToggle.getToggleConfigs());
        list.addAll(StructureToggle.getHotkeys());
        list.addAll(StructureToggle.getColorConfigs());
        this.addSubPanel((new ModConfigScreen(modId, list, "minihud.gui.button.config_gui.structures")).setConfigElementsWidth(200));

        configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values()));
        provider = new SimpleConfigInfoProvider("Hotkey to toggle the '", "' renderer");
        this.addSubPanel((new ModConfigScreen(modId, configs, "minihud.gui.button.config_gui.renderer_hotkeys")).setHoverInfoProvider(provider));
    }
}
