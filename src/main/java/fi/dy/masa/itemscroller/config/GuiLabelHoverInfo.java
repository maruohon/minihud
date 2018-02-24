package fi.dy.masa.itemscroller.config;

import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiLabel;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.resources.I18n;

public class GuiLabelHoverInfo extends GuiLabel
{
    // Stupid GuiLabel has stuff as private without getters >_>
    protected final List<String> labels;
    protected final FontRenderer fontRenderer;
    protected final int textColor;
    protected boolean labelBgEnabled;
    protected int backColor;
    protected int ulColor;
    protected int brColor;
    protected int border;
    protected int textWidth;
    protected int textHeight;

    public GuiLabelHoverInfo(FontRenderer fontRenderer, int id, int x, int y, int width, int height, int color)
    {
        super(fontRenderer, id, x, y, width, height, color);

        this.fontRenderer = fontRenderer;
        this.textColor = color;
        this.labels = new ArrayList<>();
        this.labelBgEnabled = true;
        this.backColor = 0x00000000;
        this.ulColor = 0xFFFFFFFF;
        this.brColor = 0xFFFFFFFF;
        this.border = 0;
        this.textWidth = width;
    }

    @Override
    public void addLine(String line)
    {
        String str = I18n.format(line);
        this.labels.add(str);

        this.textWidth = Math.max(this.textWidth, this.fontRenderer.getStringWidth(str));
        this.textHeight += this.fontRenderer.FONT_HEIGHT + 1;
    }

    public List<String> getText()
    {
        return this.labels;
    }

    public boolean isMouseOver(int mouseX, int mouseY)
    {
        return mouseX >= this.x && mouseX <= (this.x + this.width) && mouseY >= this.y && mouseY <= (this.y + this.height);
    }

    @Override
    public void drawLabel(Minecraft mc, int mouseX, int mouseY)
    {
        if (this.visible)
        {
            GlStateManager.enableBlend();
            GlStateManager.tryBlendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);

            //this.drawLabelBackground(mc, mouseX, mouseY);
            int rowHeight = this.fontRenderer.FONT_HEIGHT + 1;

            for (int i = 0; i < this.labels.size(); ++i)
            {
                //this.drawString(this.fontRenderer, this.labels.get(i), this.x, this.y + i * rowHeight, this.textColor);
            }
        }
    }

    protected void drawLabelBackground(Minecraft mc, int mouseX, int mouseY)
    {
        if (this.labelBgEnabled)
        {
            int totalWidth = this.textWidth + this.border * 2 + 2;
            int totalheight = this.textHeight + this.border * 2 + 2;
            int x = this.x;
            int y = this.y;

            drawRect(x, y, x + totalWidth, y + totalheight, this.backColor);

            this.drawHorizontalLine(x, x + totalWidth, y, this.ulColor);
            this.drawHorizontalLine(x, x + totalWidth, y + totalheight, this.brColor);
            this.drawVerticalLine(x, y, y + totalheight, this.ulColor);
            this.drawVerticalLine(x + totalWidth, y, y + totalheight, this.brColor);
        }
    }
}
