package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.malilib.gui.config.ModConfigScreen;
import fi.dy.masa.malilib.gui.config.liteloader.BaseConfigPanel;
import fi.dy.masa.malilib.util.data.ModInfo;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class MiniHudConfigPanel extends BaseConfigPanel
{
    @Override
    protected String getPanelTitlePrefix()
    {
        return "minihud.gui.title.configs";
    }

    @Override
    protected void createSubPanels()
    {
        ModInfo modInfo = Reference.MOD_INFO;

        this.addSubPanel((new ModConfigScreen(modInfo, Configs.Generic.OPTIONS, "minihud.gui.button.config_gui.generic")).setConfigElementsWidth(200));
        this.addSubPanel((new ModConfigScreen(modInfo, Configs.Colors.OPTIONS, "minihud.gui.button.config_gui.colors")).setConfigElementsWidth(100));
        this.addSubPanel((new ModConfigScreen(modInfo, InfoLine.VALUES, "minihud.gui.button.config_gui.info_lines")).setConfigElementsWidth(200));
        this.addSubPanel((new ModConfigScreen(modInfo, StructureToggle.VALUES, "minihud.gui.button.config_gui.structures")).setConfigElementsWidth(200));
        this.addSubPanel((new ModConfigScreen(modInfo, RendererToggle.VALUES, "minihud.gui.button.config_gui.overlay_renderers")).setConfigElementsWidth(200));
    }
}
