package fi.dy.masa.minihud.config.gui;

import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;

public class ConfigPanelInfoLineOrder extends ConfigPanelSub
{
    public ConfigPanelInfoLineOrder(MiniHudConfigPanel parent)
    {
        super("Info Line Order", parent);
    }

    @Override
    protected Configs.InfoToggle[] getConfigs()
    {
        return Configs.InfoToggle.values();
    }

    @Override
    public void addOptions(ConfigPanelHost host)
    {
        this.clearOptions();

        int x = 10;
        int y = 10;
        int configHeight = 20;
        int labelWidth = this.getMaxLabelWidth(this.getConfigs()) + 10;

        for (Configs.InfoToggle toggle : this.getConfigs())
        {
            this.addLabel(0, x, y + 7, labelWidth, 8, 0xFFFFFFFF, toggle.getName());

            ConfigTextField field = this.addTextField(0, x + labelWidth, y + 1, 200, configHeight - 3);
            field.setText(String.valueOf(toggle.getLinePosition()));
            this.addTextField(toggle, field);

            y += configHeight + 1;
        }
    }

    @Override
    protected boolean handleTextFields()
    {
        boolean dirty = false;

        for (Configs.InfoToggle config : this.getConfigs())
        {
            ConfigTextField field = this.getTextFieldFor(config);

            if (field != null)
            {
                String newValue = field.getText();
                String oldValue = String.valueOf(config.getLinePosition());

                if (newValue.equals(oldValue) == false)
                {
                    int position = -1;

                    try
                    {
                        position = Integer.parseInt(newValue);
                    }
                    catch (Exception e)
                    {
                        LiteModMiniHud.logger.warn("Invalid line position '{}' for info type '{}", newValue, config.getName(), e);
                    }

                    config.setLinePosition(position);
                    dirty = true;
                }
            }
        }

        return dirty;
    }
}
