package fi.dy.masa.itemscroller.event;

import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.ScaledResolution;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.client.event.GuiScreenEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.InventoryUtils;

public class RenderEventHandler
{
    private static final Vec3d LIGHT0_POS = (new Vec3d( 0.2D, 1.0D, -0.7D)).normalize();
    private static final Vec3d LIGHT1_POS = (new Vec3d(-0.2D, 1.0D,  0.7D)).normalize();
    private static boolean renderRecipes;

    @SubscribeEvent
    public void onDrawBackground(GuiScreenEvent.BackgroundDrawnEvent event)
    {
        if (renderRecipes && event.getGui() instanceof GuiContainer)
        {
            GuiContainer gui = (GuiContainer) event.getGui();
            RecipeStorage recipes = InputEventHandler.instance().getRecipes();
            int count = recipes.getRecipeCount();

            for (int recipeId = 0; recipeId < count; recipeId++)
            {
                this.renderStoredRecipeStack(recipeId, count, recipes.getRecipe(recipeId).getResult(),
                        gui, gui.mc, recipeId == recipes.getSelection());
            }
        }
    }

    @SubscribeEvent
    public void onDrawScreen(GuiScreenEvent.DrawScreenEvent.Post event)
    {
        if (renderRecipes && event.getGui() instanceof GuiContainer)
        {
            GuiContainer gui = (GuiContainer) event.getGui();
            RecipeStorage recipes = InputEventHandler.instance().getRecipes();
            this.renderHoverTooltip(event.getMouseX(), event.getMouseY(), recipes, gui, gui.mc);
        }
    }

    public static void setRenderStoredRecipes(boolean render)
    {
        renderRecipes = render;
    }

    public static boolean getRenderStoredRecipes()
    {
        return renderRecipes;
    }

    private void renderHoverTooltip(int mouseX, int mouseY, RecipeStorage recipes, GuiContainer gui, Minecraft mc)
    {
        ScaledResolution scaledResolution = new ScaledResolution(mc);
        final int gap = 40;
        final int recipesPerColumn = 9;
        final int stackBaseHeight = 16;
        final int usableHeight = scaledResolution.getScaledHeight() - 2 * gap; // leave a gap on the top and bottom
        // height of each entry; 9 stored recipes
        final int entryHeight = (int) (usableHeight / recipesPerColumn);
        // leave 0.25-th of a stack height gap between each entry
        final float scale = entryHeight / (stackBaseHeight * 1.25f);
        final int stackScaledSize = (int) (stackBaseHeight * scale);
        int recipeCount = recipes.getRecipeCount();

        for (int slot = 0; slot < recipeCount; slot++)
        {
            // Leave a small gap from the rendered stack to the gui's left edge
            final int columnOffsetCount = (recipeCount / recipesPerColumn) - (slot / recipesPerColumn);
            final float x = gui.getGuiLeft() - (columnOffsetCount + 0.2f) * stackScaledSize - (columnOffsetCount - 1) * scale * 20;
            final int y = (int) (gap + 0.25f * stackScaledSize + (slot % recipesPerColumn) * entryHeight);

            if (mouseX >= x && mouseX < x + stackScaledSize && mouseY >= y && mouseY < y + stackScaledSize)
            {
                ItemStack stack = recipes.getRecipe(slot).getResult();

                if (InventoryUtils.isStackEmpty(stack) == false)
                {
                    this.renderStackToolTip(mouseX, mouseY, stack, gui, mc);
                }

                break;
            }
        }
    }

    private void renderStoredRecipeStack(int recipeId, int recipeCount, ItemStack stack, GuiContainer gui, Minecraft mc, boolean selected)
    {
        FontRenderer font = getFontRenderer(mc, stack);
        final String indexStr = String.valueOf(recipeId + 1);
        final int strWidth = font.getStringWidth(indexStr);

        ScaledResolution scaledResolution = new ScaledResolution(mc);
        final int gap = 40;
        final int recipesPerColumn = 9;
        final int stackBaseHeight = 16;
        final int usableHeight = scaledResolution.getScaledHeight() - 2 * gap; // leave a gap on the top and bottom
        // height of each entry; 9 stored recipes
        final int entryHeight = (int) (usableHeight / recipesPerColumn);
        // leave 0.25-th of a stack height gap between each entry
        final float scale = entryHeight / (stackBaseHeight * 1.25f);
        final int stackScaledSize = (int) (stackBaseHeight * scale);
        // Leave a small gap from the rendered stack to the gui's left edge. The +12 is some space for the recipe's number text.
        final int columnOffsetCount = (recipeCount / recipesPerColumn) - (recipeId / recipesPerColumn);
        final float xPosition = gui.getGuiLeft() - (columnOffsetCount + 0.2f) * stackScaledSize - (columnOffsetCount - 1) * scale * 20;
        final float yPosition = gap + 0.25f * stackScaledSize + (recipeId % recipesPerColumn) * entryHeight;

        //System.out.printf("sw: %d sh: %d scale: %.3f left: %d usable h: %d entry h: %d\n",
        //        scaledResolution.getScaledWidth(), scaledResolution.getScaledHeight(), scale, guiLeft, usableHeight, entryHeight);

        GlStateManager.pushMatrix();
        GlStateManager.translate(xPosition, yPosition, 0);
        GlStateManager.scale(scale, scale, 1);
        final int w = stackBaseHeight;

        if (selected)
        {
            // Draw a light border around the selected/previously loaded recipe
            Gui.drawRect(    0,     0, w, 1, 0xFFFFFFFF);
            Gui.drawRect(    0,     0, 1, w, 0xFFFFFFFF);
            Gui.drawRect(w - 1,     0, w, w, 0xFFFFFFFF);
            Gui.drawRect(    0, w - 1, w, w, 0xFFFFFFFF);
        }

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            if (selected)
            {
                Gui.drawRect(1, 1, w - 1, w - 1, 0x20FFFFFF); // light background for the item
            }
            else
            {
                Gui.drawRect(0, 0, w, w, 0x20FFFFFF); // light background for the item
            }

            enableGUIStandardItemLighting(scale);

            stack = stack.copy();
            InventoryUtils.setStackSize(stack, 1);
            mc.getRenderItem().zLevel += 100;
            mc.getRenderItem().renderItemAndEffectIntoGUI(mc.player, stack, 0, 0);
            mc.getRenderItem().renderItemOverlayIntoGUI(font, stack, 0, 0, null);
            mc.getRenderItem().zLevel -= 100;
        }

        GlStateManager.disableBlend();
        RenderHelper.disableStandardItemLighting();
        GlStateManager.popMatrix();

        font.drawString(indexStr, (int) (xPosition - scale * strWidth), (int) (yPosition + (entryHeight - font.FONT_HEIGHT) / 2 - 2), 0xC0C0C0);
    }

    public static void enableGUIStandardItemLighting(float scale)
    {
        GlStateManager.pushMatrix();
        GlStateManager.rotate(-30.0F, 0.0F, 1.0F, 0.0F);
        GlStateManager.rotate(165.0F, 1.0F, 0.0F, 0.0F);

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
        GlStateManager.glLight(16384, 4611, RenderHelper.setColorBuffer((float) LIGHT0_POS.xCoord, (float) LIGHT0_POS.yCoord, (float) LIGHT0_POS.zCoord, 0.0f));

        float lightStrength = 0.3F * scale;
        GlStateManager.glLight(16384, 4609, RenderHelper.setColorBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.glLight(16384, 4608, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.glLight(16384, 4610, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.glLight(16385, 4611, RenderHelper.setColorBuffer((float) LIGHT1_POS.xCoord, (float) LIGHT1_POS.yCoord, (float) LIGHT1_POS.zCoord, 0.0f));
        GlStateManager.glLight(16385, 4609, RenderHelper.setColorBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.glLight(16385, 4608, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.glLight(16385, 4610, RenderHelper.setColorBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.shadeModel(7424);

        float ambientLightStrength = 0.4F;
        GlStateManager.glLightModel(2899, RenderHelper.setColorBuffer(ambientLightStrength, ambientLightStrength, ambientLightStrength, 1.0F));
    }

    private static FontRenderer getFontRenderer(Minecraft mc, ItemStack stack)
    {
        FontRenderer fontRenderer = null;

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            fontRenderer = stack.getItem().getFontRenderer(stack);
        }

        return fontRenderer != null ? fontRenderer : mc.fontRenderer;
    }

    private void renderStackToolTip(int x, int y, ItemStack stack, GuiContainer gui, Minecraft mc)
    {
        List<String> list = stack.getTooltip(mc.player, mc.gameSettings.advancedItemTooltips);

        for (int i = 0; i < list.size(); ++i)
        {
            if (i == 0)
            {
                list.set(i, stack.getRarity().rarityColor + (String)list.get(i));
            }
            else
            {
                list.set(i, TextFormatting.GRAY + (String)list.get(i));
            }
        }

        FontRenderer font = stack.getItem().getFontRenderer(stack);

        if (font == null)
        {
            font = mc.fontRenderer;
        }

        net.minecraftforge.fml.client.config.GuiUtils.preItemToolTip(stack);
        net.minecraftforge.fml.client.config.GuiUtils.drawHoveringText(list, x, y, gui.width, gui.height, -1, font);
        net.minecraftforge.fml.client.config.GuiUtils.postItemToolTip();
    }
}
