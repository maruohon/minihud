package fi.dy.masa.itemscroller.config;

import java.util.ArrayList;
import java.util.List;
import com.mumfrey.liteloader.modconfig.AbstractConfigPanel;
import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.TextFormatting;

public class ItemScrollerConfigPanel extends AbstractConfigPanel
{
    private final ConfigOptionListenerBoolean listener = new ConfigOptionListenerBoolean();
    private final List<HoverInfo> configComments = new ArrayList<>();

    @Override
    public String getPanelTitle()
    {
        return "Item Scroller options";
    }

    @Override
    public void onPanelHidden()
    {
        if (this.listener.isDirty())
        {
            Configs.save();
            this.listener.resetModified();
        }
    }

    @Override
    protected void addOptions(ConfigPanelHost host)
    {
        int x = 10;
        int y = 10;
        int labelWidth = 200;
        int i = 0;

        for (Configs.Toggles toggle : Configs.Toggles.values())
        {
            this.addLabel(i, x, y + 6, labelWidth, 8, 0xFFFFFFFF, toggle.getName());
            String comment = toggle.getComment();

            if (comment != null)
            {
                this.addConfigComment(x, y + 2, labelWidth, 10, comment);
            }

            this.addControl(new ConfigButtonBoolean(i + 1, x + labelWidth + 2, y, toggle), this.listener);
            i += 2;
            y += 21;
        }
    }

    protected void addConfigComment(int x, int y, int width, int height, String comment)
    {
        HoverInfo info = new HoverInfo(x, y, width, height);
        info.addLines(comment);
        this.configComments.add(info);
    }

    @Override
    public void drawPanel(ConfigPanelHost host, int mouseX, int mouseY, float partialTicks)
    {
        super.drawPanel(host, mouseX, mouseY, partialTicks);

        for (HoverInfo label : this.configComments)
        {
            if (label.isMouseOver(mouseX, mouseY))
            {
                this.drawHoveringText(label.getLines(), label.x, label.y + 30);
                break;
            }
        }
    }

    private class ConfigOptionListenerBoolean implements ConfigOptionListener<ConfigButtonBoolean>
    {
        private boolean dirty;

        @Override
        public void actionPerformed(ConfigButtonBoolean control)
        {
            control.onMouseClicked();
            this.dirty = true;
        }

        public boolean isDirty()
        {
            return this.dirty;
        }

        private void resetModified()
        {
            this.dirty = false;
        }
    }

    private class ConfigButtonBoolean extends GuiButton
    {
        private final Configs.Toggles toggle;

        public ConfigButtonBoolean(int id, int x, int y, Configs.Toggles toggle)
        {
            super(id, x, y, 100, 20, "");
            this.toggle = toggle;
            this.updateDisplayString();
        }

        public void onMouseClicked()
        {
            this.toggle.setValue(! this.toggle.getValue());
            this.updateDisplayString();
        }

        public void updateDisplayString()
        {
            String valueStr = String.valueOf(this.toggle.getValue());

            if (this.toggle.getValue())
            {
                this.displayString = TextFormatting.DARK_GREEN + valueStr + TextFormatting.RESET;
            }
            else
            {
                this.displayString = TextFormatting.DARK_RED + valueStr + TextFormatting.RESET;
            }
        }
    }

    private class HoverInfo
    {
        protected final List<String> lines;
        protected int x;
        protected int y;
        protected int width;
        protected int height;

        public HoverInfo(int x, int y, int width, int height)
        {
            this.lines = new ArrayList<>();
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }

        public void addLines(String... lines)
        {
            for (String line : lines)
            {
                line = I18n.format(line);
                String[] split = line.split("\n");

                for (String str : split)
                {
                    this.lines.add(str);
                }
            }
        }

        public List<String> getLines()
        {
            return this.lines;
        }

        public boolean isMouseOver(int mouseX, int mouseY)
        {
            return mouseX >= this.x && mouseX <= (this.x + this.width) && mouseY >= this.y && mouseY <= (this.y + this.height);
        }
    }
}
