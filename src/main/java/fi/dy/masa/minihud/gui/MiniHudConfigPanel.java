package fi.dy.masa.minihud.gui;

import fi.dy.masa.malilib.gui.config.liteloader.RedirectingConfigPanel;

public class MiniHudConfigPanel extends RedirectingConfigPanel
{
    public MiniHudConfigPanel()
    {
        super(ConfigScreen::create);
    }
}
