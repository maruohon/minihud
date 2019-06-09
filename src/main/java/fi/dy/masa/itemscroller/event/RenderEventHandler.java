package fi.dy.masa.itemscroller.event;

import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.recipes.CraftingRecipe;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.malilib.render.InventoryOverlay;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.StringUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.Vec3d;


public class RenderEventHandler
{
    private static final RenderEventHandler INSTANCE = new RenderEventHandler();
    private static final Vec3d LIGHT0_POS = (new Vec3d( 0.2D, 1.0D, -0.7D)).normalize();
    private static final Vec3d LIGHT1_POS = (new Vec3d(-0.2D, 1.0D,  0.7D)).normalize();

    private final Minecraft mc = Minecraft.getInstance();
    private int recipeListX;
    private int recipeListY;
    private int recipesPerColumn;
    private int columnWidth;
    private int columns;
    private int numberTextWidth;
    private int gapColumn;
    private int entryHeight;
    private double scale;

    public static RenderEventHandler instance()
    {
        return INSTANCE;
    }

    public void onDrawBackgroundPost()
    {
        if (GuiUtils.getCurrentScreen() instanceof GuiContainer && InputUtils.isRecipeViewOpen())
        {
            GuiContainer gui = (GuiContainer) GuiUtils.getCurrentScreen();
            RecipeStorage recipes = RecipeStorage.getInstance();
            final int first = recipes.getFirstVisibleRecipeId();
            final int countPerPage = recipes.getRecipeCountPerPage();
            final int lastOnPage = first + countPerPage - 1;

            this.calculateRecipePositions(gui);

            GlStateManager.pushMatrix();
            GlStateManager.translatef(this.recipeListX, this.recipeListY, 0);
            GlStateManager.scaled(this.scale, this.scale, 1);

            String str = StringUtils.translate("itemscroller.gui.label.recipe_page", (first / countPerPage) + 1, recipes.getTotalRecipeCount() / countPerPage);
            this.mc.fontRenderer.drawString(str, 16, -12, 0xC0C0C0C0);

            for (int i = 0, recipeId = first; recipeId <= lastOnPage; ++i, ++recipeId)
            {
                ItemStack stack = recipes.getRecipe(recipeId).getResult();
                boolean selected = recipeId == recipes.getSelection();
                int row = i % this.recipesPerColumn;
                int column = i / this.recipesPerColumn;

                this.renderStoredRecipeStack(stack, recipeId, row, column, gui, selected);
            }

            if (Configs.Generic.CRAFTING_RENDER_RECIPE_ITEMS.getBooleanValue())
            {
                final int mouseX = fi.dy.masa.malilib.util.InputUtils.getMouseX();
                final int mouseY = fi.dy.masa.malilib.util.InputUtils.getMouseY();
                final int recipeId = this.getHoveredRecipeId(mouseX, mouseY, recipes, gui);
                CraftingRecipe recipe = recipeId >= 0 ? recipes.getRecipe(recipeId) : recipes.getSelectedRecipe();

                this.renderRecipeItems(recipe, recipes.getRecipeCountPerPage(), gui);
            }

            GlStateManager.popMatrix();
            GlStateManager.enableBlend(); // Fixes the crafting book icon rendering
        }
    }

    public void onDrawScreenPost()
    {
        if (GuiUtils.getCurrentScreen() instanceof GuiContainer && InputUtils.isRecipeViewOpen())
        {
            GuiContainer gui = (GuiContainer) GuiUtils.getCurrentScreen();
            RecipeStorage recipes = RecipeStorage.getInstance();

            final int mouseX = fi.dy.masa.malilib.util.InputUtils.getMouseX();
            final int mouseY = fi.dy.masa.malilib.util.InputUtils.getMouseY();
            final int recipeId = this.getHoveredRecipeId(mouseX, mouseY, recipes, gui);

            if (recipeId >= 0)
            {
                CraftingRecipe recipe = recipes.getRecipe(recipeId);
                this.renderHoverTooltip(mouseX, mouseY, recipe, gui);
            }
            else if (Configs.Generic.CRAFTING_RENDER_RECIPE_ITEMS.getBooleanValue())
            {
                CraftingRecipe recipe = recipes.getSelectedRecipe();
                ItemStack stack = this.getHoveredRecipeIngredient(mouseX, mouseY, recipe, recipes.getRecipeCountPerPage(), gui);

                if (InventoryUtils.isStackEmpty(stack) == false)
                {
                    InventoryOverlay.renderStackToolTip(mouseX, mouseY, stack, this.mc);
                }
            }
        }
    }

    private void calculateRecipePositions(GuiContainer gui)
    {
        RecipeStorage recipes = RecipeStorage.getInstance();
        final int gapHorizontal = 2;
        final int gapVertical = 2;
        final int stackBaseHeight = 16;
        final int guiLeft = AccessorUtils.getGuiLeft(gui);

        this.recipesPerColumn = 9;
        this.columns = (int) Math.ceil((double) recipes.getRecipeCountPerPage() / (double) this.recipesPerColumn);
        this.numberTextWidth = 12;
        this.gapColumn = 4;

        int usableHeight = GuiUtils.getScaledWindowHeight();
        int usableWidth = guiLeft;
        // Scale the maximum stack size by taking into account the relative gap size
        double gapScaleVertical = (1D - (double) gapVertical / (double) (stackBaseHeight + gapVertical));
        // the +1.2 is for the gap and page text height on the top and bottom
        int maxStackDimensionsVertical = (int) ((usableHeight / ((double) this.recipesPerColumn + 1.2)) * gapScaleVertical);
        // assume a maximum of 3x3 recipe size for now... thus columns + 3 stacks rendered horizontally
        double gapScaleHorizontal = (1D - (double) gapHorizontal / (double) (stackBaseHeight + gapHorizontal));
        int maxStackDimensionsHorizontal = (int) (((usableWidth - (this.columns * (this.numberTextWidth + this.gapColumn))) / (this.columns + 3 + 0.8)) * gapScaleHorizontal);
        int stackDimensions = (int) Math.min(maxStackDimensionsVertical, maxStackDimensionsHorizontal);

        this.scale = (int) Math.ceil(((double) stackDimensions / (double) stackBaseHeight));
        this.entryHeight = stackBaseHeight + gapVertical;
        this.recipeListX = guiLeft - (int) ((this.columns * (stackBaseHeight + this.numberTextWidth + this.gapColumn) + gapHorizontal) * this.scale);
        this.recipeListY = (int) (this.entryHeight * this.scale);
        this.columnWidth = stackBaseHeight + this.numberTextWidth + this.gapColumn;
    }

    private void renderHoverTooltip(int mouseX, int mouseY, CraftingRecipe recipe, GuiContainer gui)
    {
        ItemStack stack = recipe.getResult();

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            InventoryOverlay.renderStackToolTip(mouseX, mouseY, stack, this.mc);
        }
    }

    public int getHoveredRecipeId(int mouseX, int mouseY, RecipeStorage recipes, GuiContainer gui)
    {
        if (InputUtils.isRecipeViewOpen())
        {
            this.calculateRecipePositions(gui);
            final int stackDimensions = (int) (16 * this.scale);

            for (int column = 0; column < this.columns; ++column)
            {
                int startX = this.recipeListX + (int) ((column * this.columnWidth + this.gapColumn + this.numberTextWidth) * this.scale);

                if (mouseX >= startX && mouseX <= startX + stackDimensions)
                {
                    for (int row = 0; row < this.recipesPerColumn; ++row)
                    {
                        int startY = this.recipeListY + (int) (row * this.entryHeight * this.scale);

                        if (mouseY >= startY && mouseY <= startY + stackDimensions)
                        {
                            return recipes.getFirstVisibleRecipeId() + column * this.recipesPerColumn + row;
                        }
                    }
                }
            }
        }

        return -1;
    }

    private void renderStoredRecipeStack(ItemStack stack, int recipeId, int row, int column, GuiContainer gui, boolean selected)
    {
        final FontRenderer font = this.mc.fontRenderer;
        final String indexStr = String.valueOf(recipeId + 1);

        int x = column * this.columnWidth + this.gapColumn + this.numberTextWidth;
        int y = row * this.entryHeight;
        this.renderStackAt(stack, x, y, selected);

        double scale = 0.75;
        x = x - (int) (font.getStringWidth(indexStr) * scale) - 2;
        y = row * this.entryHeight + this.entryHeight / 2 - font.FONT_HEIGHT / 2;

        GlStateManager.pushMatrix();
        GlStateManager.translatef(x, y, 0);
        GlStateManager.scaled(scale, scale, 0);

        font.drawString(indexStr, 0, 0, 0xC0C0C0);

        GlStateManager.popMatrix();
    }

    private void renderRecipeItems(CraftingRecipe recipe, int recipeCountPerPage, GuiContainer gui)
    {
        ItemStack[] items = recipe.getRecipeItems();
        final int recipeDimensions = (int) Math.ceil(Math.sqrt(recipe.getRecipeLength()));
        int x = -3 * 17 + 2;
        int y = 3 * this.entryHeight;

        for (int i = 0, row = 0; row < recipeDimensions; row++)
        {
            for (int col = 0; col < recipeDimensions; col++, i++)
            {
                int xOff = col * 17;
                int yOff = row * 17;

                this.renderStackAt(items[i], x + xOff, y + yOff, false);
            }
        }
    }

    private ItemStack getHoveredRecipeIngredient(int mouseX, int mouseY, CraftingRecipe recipe, int recipeCountPerPage, GuiContainer gui)
    {
        final int recipeDimensions = (int) Math.ceil(Math.sqrt(recipe.getRecipeLength()));
        int scaledStackDimensions = (int) (16 * this.scale);
        int scaledGridEntry = (int) (17 * this.scale);
        int x = this.recipeListX - (int) ((3 * 17 - 2) * this.scale);
        int y = this.recipeListY + (int) (3 * this.entryHeight * this.scale);

        if (mouseX >= x && mouseX <= x + recipeDimensions * scaledGridEntry &&
            mouseY >= y && mouseY <= y + recipeDimensions * scaledGridEntry)
        {
            for (int i = 0, row = 0; row < recipeDimensions; row++)
            {
                for (int col = 0; col < recipeDimensions; col++, i++)
                {
                    int xOff = col * scaledGridEntry;
                    int yOff = row * scaledGridEntry;
                    int xStart = x + xOff;
                    int yStart = y + yOff;

                    if (mouseX >= xStart && mouseX < xStart + scaledStackDimensions &&
                        mouseY >= yStart && mouseY < yStart + scaledStackDimensions)
                    {
                        return recipe.getRecipeItems()[i];
                    }
                }
            }
        }

        return ItemStack.EMPTY;
    }

    private void renderStackAt(ItemStack stack, int x, int y, boolean border)
    {
        GlStateManager.pushMatrix();
        GlStateManager.disableLighting();
        final int w = 16;

        if (border)
        {
            // Draw a light/white border around the stack
            RenderUtils.drawRect(x - 1, y - 1, w + 1, 1    , 0xFFFFFFFF);
            RenderUtils.drawRect(x - 1, y    , 1    , w + 1, 0xFFFFFFFF);
            RenderUtils.drawRect(x + w, y - 1, 1    , w + 1, 0xFFFFFFFF);
            RenderUtils.drawRect(x    , y + w, w + 1, 1    , 0xFFFFFFFF);

            RenderUtils.drawRect(x, y, w, w, 0x20FFFFFF); // light background for the item

        }
        else
        {
            RenderUtils.drawRect(x, y, w, w, 0x20FFFFFF); // light background for the item
        }

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            enableGUIStandardItemLighting((float) this.scale);

            stack = stack.copy();
            InventoryUtils.setStackSize(stack, 1);
            this.mc.getItemRenderer().zLevel += 100;
            this.mc.getItemRenderer().renderItemAndEffectIntoGUI(this.mc.player, stack, x, y);
            this.mc.getItemRenderer().zLevel -= 100;
        }

        RenderUtils.disableItemLighting();
        GlStateManager.disableBlend();
        GlStateManager.popMatrix();
    }

    public static void enableGUIStandardItemLighting(float scale)
    {
        GlStateManager.pushMatrix();
        GlStateManager.rotatef(-30.0F, 0.0F, 1.0F, 0.0F);
        GlStateManager.rotatef(165.0F, 1.0F, 0.0F, 0.0F);

        enableStandardItemLighting(scale);

        GlStateManager.popMatrix();
    }

    public static void enableStandardItemLighting(float scale)
    {
        GlStateManager.enableLighting();
        GlStateManager.enableLight(0);
        GlStateManager.enableLight(1);
        GlStateManager.enableColorMaterial();
        GlStateManager.colorMaterial(1032, 5634);
        GlStateManager.lightfv(16384, 4611, RenderHelper.setColorBuffer((float) LIGHT0_POS.x, (float) LIGHT0_POS.y, (float) LIGHT0_POS.z, 0.0f));

        float lightStrength = 0.3F * scale;
        GlStateManager.lightfv(16384, 4609, RenderHelper.setColorBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.lightfv(16384, 4608, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.lightfv(16384, 4610, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.lightfv(16385, 4611, RenderHelper.setColorBuffer((float) LIGHT1_POS.x, (float) LIGHT1_POS.y, (float) LIGHT1_POS.z, 0.0f));
        GlStateManager.lightfv(16385, 4609, RenderHelper.setColorBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.lightfv(16385, 4608, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.lightfv(16385, 4610, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.shadeModel(7424);

        float ambientLightStrength = 0.4F;
        GlStateManager.lightModelfv(2899, RenderHelper.setColorBuffer(ambientLightStrength, ambientLightStrength, ambientLightStrength, 1.0F));
    }
}
