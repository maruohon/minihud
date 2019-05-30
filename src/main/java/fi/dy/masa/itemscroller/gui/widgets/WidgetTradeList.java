package fi.dy.masa.itemscroller.gui.widgets;

import java.util.ArrayList;
import java.util.List;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.gui.GuiScrollBar;
import fi.dy.masa.malilib.gui.widgets.WidgetBase;
import fi.dy.masa.malilib.render.RenderUtils;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.client.resources.I18n;
import net.minecraft.village.MerchantRecipe;
import net.minecraft.village.MerchantRecipeList;

public class WidgetTradeList extends WidgetBase
{
    private final GuiScrollBar scrollBar;
    private final GuiMerchant parentGui;
    private final VillagerDataStorage storage;
    private final ArrayList<WidgetTradeEntry> entryList = new ArrayList<>();
    private final VillagerData data;
    private MerchantRecipeList recipeList;
    private int scrollBarTotalHeight;

    public WidgetTradeList(int x, int y, GuiMerchant parentGui, VillagerData data)
    {
        super(x, y, 106, 166);

        this.scrollBar = (new GuiScrollBar(Icons.SCROLL_BAR_6)).setRenderBarBackground(false);
        this.parentGui = parentGui;
        this.storage = VillagerDataStorage.getInstance();
        this.data = data;
    }

    private void lazySetRecipeList()
    {
        if (this.recipeList == null)
        {
            this.recipeList = this.parentGui.getMerchant().getRecipes(this.mc.player);

            if (this.recipeList != null)
            {
                int max = Math.max(0, this.recipeList.size() - 7);
                this.scrollBar.setMaxValue(max);
                this.scrollBar.setValue(this.data.getTradeListPosition());
                this.scrollBarTotalHeight = Math.max(140, this.recipeList.size() * 20);

                this.reCreateEntryWidgets();
            }
        }
    }

    @Override
    public boolean isMouseOver(int mouseX, int mouseY)
    {
        return mouseX >= this.x +  5 && mouseX <= this.x + 99 &&
               mouseY >= this.y + 18 && mouseY <= this.y + 157;
    }

    @Override
    protected boolean onMouseClickedImpl(int mouseX, int mouseY, int mouseButton)
    {
        if (this.scrollBar.wasMouseOver())
        {
            this.scrollBar.setIsDragging(true);
            return true;
        }

        if (mouseX <= this.x + 92)
        {
            int relY = mouseY - (this.y + 18);
            int listIndex = relY / 20;
            WidgetTradeEntry entry = listIndex >= 0 && listIndex < this.entryList.size() ? this.entryList.get(listIndex) : null;
            int recipeIndex = entry != null ? entry.getListIndex() : -1;

            if (recipeIndex >= 0)
            {
                // Middle click to toggle favorites
                if (mouseButton == 2)
                {
                    this.storage.toggleFavorite(recipeIndex);
                    this.reCreateEntryWidgets();
                }
                else
                {
                    boolean samePage = AccessorUtils.getSelectedMerchantRecipe(this.parentGui) == recipeIndex;
                    InputHandler.changeTradePage(this.parentGui, recipeIndex);

                    if (GuiScreen.isShiftKeyDown() || samePage || mouseButton == 1)
                    {
                        InventoryUtils.villagerClearTradeInputSlots();

                        if (mouseButton == 1)
                        {
                            InventoryUtils.villagerTradeEverythingPossibleWithCurrentRecipe();
                        }
                        else
                        {
                            InventoryUtils.tryMoveItemsToMerchantBuySlots(this.parentGui, true);
                        }
                    }
                }
            }
        }

        return true;
    }

    @Override
    public void onMouseReleasedImpl(int mouseX, int mouseY, int mouseButton)
    {
        this.scrollBar.setIsDragging(false);
    }

    @Override
    public boolean onMouseScrolledImpl(int mouseX, int mouseY, double mouseWheelDelta)
    {
        this.scrollBar.offsetValue(mouseWheelDelta < 0 ? 1 : -1);
        return true;
    }

    @Override
    public void render(int mouseX, int mouseY, boolean selected)
    {
        this.lazySetRecipeList();

        if (this.recipeList != null)
        {
            int currentPage = AccessorUtils.getSelectedMerchantRecipe(this.parentGui);
            currentPage = Math.min(currentPage, this.recipeList.size() - 1);
            this.updateDataStorage(currentPage);

            GlStateManager.color4f(1f, 1f, 1f, 1f);
            RenderHelper.disableStandardItemLighting();
            this.bindTexture(Icons.TEXTURE);

            // Background
            RenderUtils.drawTexturedRect(this.x, this.y, 0, 0, this.width, 166);

            String str = I18n.format("itemscroller.gui.label.trades");
            int w = this.textRenderer.getStringWidth(str);
            this.drawString(str, this.x + this.width / 2 - w / 2, this.y + 6, 0xFF404040);

            this.scrollBar.render(mouseX, mouseY, 0, this.x + 93, this.y + 17, 8, 142, this.scrollBarTotalHeight);

            // Render the trades
            for (WidgetTradeEntry entry : this.entryList)
            {
                entry.render(mouseX, mouseY, currentPage == entry.getListIndex());
            }

            for (WidgetTradeEntry entry : this.entryList)
            {
                if (entry.isMouseOver(mouseX, mouseY))
                {
                    entry.postRenderHovered(mouseX, mouseY, false);
                }
            }
        }
    }

    private void reCreateEntryWidgets()
    {
        if (this.recipeList != null)
        {
            this.entryList.clear();

            ArrayList<MerchantRecipe> list = new ArrayList<>();
            List<Integer> favorites = this.data.getFavorites();

            // Some favorites defined
            if (favorites.isEmpty() == false)
            {
                // First pick all the favorited recipes, in the order they are in the favorites list
                for (int index : favorites)
                {
                    if (index >= 0 && index < this.recipeList.size())
                    {
                        list.add(this.recipeList.get(index));
                    }
                }

                // Then add the rest of the recipes in their original order
                for (int i = 0; i < this.recipeList.size(); ++i)
                {
                    if (favorites.contains(i) == false)
                    {
                        list.add(this.recipeList.get(i));
                    }
                }
            }
            else
            {
                list.addAll(this.recipeList);
            }

            final int scrollBarPos = this.scrollBar.getValue();
            final int last = Math.min(scrollBarPos + 7, list.size());
            final int x = this.x + 5;

            for (int index = scrollBarPos; index < last; ++index)
            {
                int y = this.y + (index - scrollBarPos) * 20 + 18;
                MerchantRecipe recipe = list.get(index);

                this.entryList.add(new WidgetTradeEntry(x, y, 88, 20, recipe, this.recipeList.indexOf(recipe), this.data));
            }
        }
    }

    private void updateDataStorage(int currentPage)
    {
        int oldPosition = this.data.getTradeListPosition();
        int newPosition = this.scrollBar.getValue();

        if (this.data.getLastPage() != currentPage)
        {
            this.storage.setLastPage(currentPage);
        }

        if (newPosition != oldPosition)
        {
            this.storage.setTradeListPosition(newPosition);
            this.reCreateEntryWidgets();
        }
    }
}
