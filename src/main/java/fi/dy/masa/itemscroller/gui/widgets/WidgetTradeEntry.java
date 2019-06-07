package fi.dy.masa.itemscroller.gui.widgets;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.interfaces.IGuiIcon;
import fi.dy.masa.malilib.gui.widgets.WidgetListEntryBase;
import fi.dy.masa.malilib.render.InventoryOverlay;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.StringUtils;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.village.MerchantRecipe;

public class WidgetTradeEntry extends WidgetListEntryBase<MerchantRecipe>
{
    public static final ResourceLocation BUTTON_TEXTURE = new ResourceLocation("textures/gui/widgets.png");

    private final VillagerData data;

    public WidgetTradeEntry(int x, int y, int width, int height,
            MerchantRecipe entry, int listIndex, VillagerData data)
    {
        super(x, y, width, height, entry, listIndex);

        this.data = data;
    }

    @Override
    public void render(int mouseX, int mouseY, boolean selected)
    {
        RenderUtils.color(1f, 1f, 1f, 1f);

        this.bindTexture(BUTTON_TEXTURE);

        int v = 66;

        if (this.isMouseOver(mouseX, mouseY))
        {
            v += 20;
        }

        // Button background texture for the trades
        RenderUtils.drawTexturedRect(this.x                 , this.y,   0, v, this.width - 4, this.height);
        RenderUtils.drawTexturedRect(this.x + this.width - 4, this.y, 196, v,              4, this.height);

        if (selected)
        {
            RenderUtils.drawOutline(this.x, this.y, this.width, this.height, 0xFFFFB000, 1f);
        }

        this.bindTexture(Icons.TEXTURE);

        IGuiIcon icon = this.entry.isRecipeDisabled() ? Icons.TRADE_ARROW_LOCKED : Icons.TRADE_ARROW_AVAILABLE;

        RenderUtils.color(1f, 1f, 1f, 1f);
        RenderUtils.setupBlend();
        GlStateManager.enableAlpha();

        // Trade arrow
        icon.renderAt(this.x + 44, this.y + 5, 1f, false, false);

        // This entry has been favorited
        if (this.data.getFavorites().contains(this.getListIndex()))
        {
            Icons.STAR_5.renderAt(this.x + 80, this.y + 2, 1f, false, false);
        }

        GlStateManager.disableBlend();

        ItemStack buy1 = this.entry.getItemToBuy();
        ItemStack buy2 = this.entry.getSecondItemToBuy();
        ItemStack sell = this.entry.getItemToSell();

        if (buy1.isEmpty() == false)
        {
            InventoryOverlay.renderStackAt(buy1, this.x +  4, this.y + 2, 1, this.mc);
        }

        if (buy2.isEmpty() == false)
        {
            InventoryOverlay.renderStackAt(buy2, this.x + 22, this.y + 2, 1, this.mc);
        }

        if (sell.isEmpty() == false)
        {
            InventoryOverlay.renderStackAt(sell, this.x + 60, this.y + 2, 1, this.mc);
        }
    }

    @Override
    public void postRenderHovered(int mouseX, int mouseY, boolean selected)
    {
        if (mouseY >= this.y + 2 && mouseY <= this.y + this.height - 2)
        {
            if (mouseX >= this.x + 4 && mouseX <= this.x + 4 + 16)
            {
                ItemStack buy1 = this.entry.getItemToBuy();

                if (buy1.isEmpty() == false)
                {
                    InventoryOverlay.renderStackToolTip(mouseX, mouseY, buy1, this.mc);
                }
            }
            else if (mouseX >= this.x + 22 && mouseX <= this.x + 22 + 16)
            {
                ItemStack buy2 = this.entry.getSecondItemToBuy();

                if (buy2.isEmpty() == false)
                {
                    InventoryOverlay.renderStackToolTip(mouseX, mouseY, buy2, this.mc);
                }
            }
            else if (mouseX >= this.x + 60 && mouseX <= this.x + 60 + 16)
            {
                ItemStack sell = this.entry.getItemToSell();

                if (sell.isEmpty() == false)
                {
                    InventoryOverlay.renderStackToolTip(mouseX, mouseY, sell, this.mc);
                }
            }

            if (GuiBase.isAltDown())
            {
                int uses = this.entry.getToolUses();
                int max = this.entry.getMaxTradeUses();
                RenderUtils.drawHoverText(mouseX + 6, mouseY + 18, ImmutableList.of(StringUtils.translate("itemscroller.gui.label.trade_uses", uses, max)));
            }
        }
    }
}
