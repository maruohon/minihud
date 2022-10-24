package fi.dy.masa.minihud.gui;

import malilib.gui.config.liteloader.RedirectingConfigPanel;

public class MiniHudConfigPanel extends RedirectingConfigPanel
{
    public MiniHudConfigPanel()
    {
        super(ConfigScreen::create);
    }
}
