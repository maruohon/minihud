package fi.dy.masa.itemscroller.event;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nonnull;
import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.recipes.RecipePattern;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.font.TextRenderer;
import net.minecraft.client.gui.ContainerScreen;
import net.minecraft.client.gui.DrawableHelper;
import net.minecraft.client.item.TooltipContext;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.GuiLighting;
import net.minecraft.client.render.Tessellator;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.util.Window;
import net.minecraft.item.ItemStack;
import net.minecraft.text.StringTextComponent;
import net.minecraft.text.TextComponent;
import net.minecraft.text.TextFormat;
import net.minecraft.util.math.Vec3d;

public class RenderEventHandler
{
    private static final RenderEventHandler INSTANCE = new RenderEventHandler();
    private static final Vec3d LIGHT0_POS = (new Vec3d( 0.2D, 1.0D, -0.7D)).normalize();
    private static final Vec3d LIGHT1_POS = (new Vec3d(-0.2D, 1.0D,  0.7D)).normalize();

    public static RenderEventHandler instance()
    {
        return INSTANCE;
    }

    public void onDrawBackgroundPost()
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc.currentScreen instanceof ContainerScreen && InputUtils.isRecipeViewOpen())
        {
            ContainerScreen<?> gui = (ContainerScreen<?>) mc.currentScreen;
            RecipeStorage recipes = KeybindCallbacks.getInstance().getRecipes();
            final int count = recipes.getRecipeCount();

            for (int recipeId = 0; recipeId < count; recipeId++)
            {
                this.renderStoredRecipeStack(recipeId, count, recipes.getRecipe(recipeId).getResult(),
                        gui, mc, recipeId == recipes.getSelection());
            }
        }
    }

    public void onDrawScreenPost()
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc.currentScreen instanceof ContainerScreen && InputUtils.isRecipeViewOpen())
        {
            ContainerScreen<?> gui = (ContainerScreen<?>) mc.currentScreen;
            RecipeStorage recipes = KeybindCallbacks.getInstance().getRecipes();

            Window window = mc.window;
            final int mouseX = (int) (mc.mouse.getX() * (double) window.getScaledWidth() / (double) window.getWidth());
            final int mouseY = (int) (mc.mouse.getY() * (double) window.getScaledHeight() / (double) window.getHeight());
            final int recipeId = this.getHoveredRecipeId(mouseX, mouseY, recipes, gui, mc);

            if (recipeId >= 0)
            {
                RecipePattern recipe = recipes.getRecipe(recipeId);

                if (Configs.Generic.CRAFTING_RENDER_RECIPE_ITEMS.getBooleanValue())
                {
                    this.renderRecipeItems(recipe, recipes.getRecipeCount(), gui, mc);
                }

                this.renderHoverTooltip(mouseX, mouseY, recipe, gui, mc);
            }
            else if (Configs.Generic.CRAFTING_RENDER_RECIPE_ITEMS.getBooleanValue())
            {
                RecipePattern recipe = recipes.getSelectedRecipe();
                this.renderRecipeItems(recipe, recipes.getRecipeCount(), gui, mc);

                ItemStack stack = this.getHoveredRecipeIngredient(mouseX, mouseY, recipe, recipes.getRecipeCount(), gui, mc);

                if (InventoryUtils.isStackEmpty(stack) == false)
                {
                    this.renderStackToolTip(mouseX, mouseY, stack, gui, mc);
                }
            }
        }
    }

    private void renderHoverTooltip(int mouseX, int mouseY, RecipePattern recipe, ContainerScreen<?> gui, MinecraftClient mc)
    {
        ItemStack stack = recipe.getResult();

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            this.renderStackToolTip(mouseX, mouseY, stack, gui, mc);
        }
    }

    public int getHoveredRecipeId(int mouseX, int mouseY, RecipeStorage recipes, ContainerScreen<?> gui, MinecraftClient mc)
    {
        if (InputUtils.isRecipeViewOpen())
        {
            Window window = mc.window;
            final int gap = 40;
            final int recipesPerColumn = 9;
            final int stackBaseHeight = 16;
            final int usableHeight = window.getScaledHeight() - 2 * gap; // leave a gap on the top and bottom
            // height of each entry; 9 stored recipes
            final int entryHeight = (int) (usableHeight / recipesPerColumn);
            // leave 0.25-th of a stack height gap between each entry
            final float scale = entryHeight / (stackBaseHeight * 1.25f);
            final int stackScaledSize = (int) (stackBaseHeight * scale);
            final int recipeCount = recipes.getRecipeCount();

            for (int recipeId = 0; recipeId < recipeCount; recipeId++)
            {
                // Leave a small gap from the rendered stack to the gui's left edge
                final int columnOffsetCount = (recipeCount / recipesPerColumn) - (recipeId / recipesPerColumn);
                final float x = AccessorUtils.getGuiLeft(gui) - (columnOffsetCount + 0.2f) * stackScaledSize - (columnOffsetCount - 1) * scale * 20;
                final int y = (int) (gap + 0.25f * stackScaledSize + (recipeId % recipesPerColumn) * entryHeight);

                if (mouseX >= x && mouseX < x + stackScaledSize && mouseY >= y && mouseY < y + stackScaledSize)
                {
                    return recipeId;
                }
            }
        }

        return -1;
    }

    private void renderStoredRecipeStack(int recipeId, int recipeCount, ItemStack stack, ContainerScreen<?> gui, MinecraftClient mc, boolean selected)
    {
        TextRenderer font = mc.textRenderer;
        final String indexStr = String.valueOf(recipeId + 1);
        final int strWidth = font.getStringWidth(indexStr);

        Window window = mc.window;
        final int verticalGap = 40;
        final int recipesPerColumn = 9;
        final int stackBaseHeight = 16;
        final int usableHeight = window.getScaledHeight() - 2 * verticalGap; // leave a gap on the top and bottom
        // height of each entry; 9 stored recipes
        final int entryHeight = (int) (usableHeight / recipesPerColumn);
        // leave 0.25-th of a stack height gap between each entry
        final float scale = entryHeight / (stackBaseHeight * 1.25f);
        final int stackScaledSize = (int) (stackBaseHeight * scale);
        // Leave a small gap from the rendered stack to the gui's left edge. The +12 is some space for the recipe's number text.
        final int columnOffsetCount = (recipeCount / recipesPerColumn) - (recipeId / recipesPerColumn);
        final float x = AccessorUtils.getGuiLeft(gui) - (columnOffsetCount + 0.2f) * stackScaledSize - (columnOffsetCount - 1) * scale * 20;
        final float y = verticalGap + 0.25f * stackScaledSize + (recipeId % recipesPerColumn) * entryHeight;

        //System.out.printf("sw: %d sh: %d scale: %.3f left: %d usable h: %d entry h: %d\n",
        //        scaledResolution.getScaledWidth(), scaledResolution.getScaledHeight(), scale, guiLeft, usableHeight, entryHeight);

        this.renderStackAt(stack, x, y, selected, scale, stackBaseHeight, mc);

        font.draw(indexStr, (int) (x - scale * strWidth), (int) (y + (entryHeight - font.fontHeight) / 2 - 2), 0xC0C0C0);
    }

    private void renderRecipeItems(RecipePattern recipe, int recipeCount, ContainerScreen<?> gui, MinecraftClient mc)
    {
        RecipeLocation location = this.getRecipeLocation(recipe, recipeCount, gui, mc);

        ItemStack[] items = recipe.getRecipeItems();

        for (int i = 0, row = 0; row < location.recipeDimensions; row++)
        {
            for (int col = 0; col < location.recipeDimensions; col++, i++)
            {
                float xOff = col * ((location.stackBaseHeight + 1) * location.scale);
                float yOff = row * ((location.stackBaseHeight + 1) * location.scale);
                this.renderStackAt(items[i], location.x + xOff, location.y + yOff, false, location.scale, location.stackBaseHeight, mc);
            }
        }
    }

    private RecipeLocation getRecipeLocation(RecipePattern recipe, int recipeCount, ContainerScreen<?> gui, MinecraftClient mc)
    {
        Window window = mc.window;
        final int verticalGap = 40;
        final int recipesPerColumn = 9;
        final int stackBaseHeight = 16;
        final int usableHeight = window.getScaledHeight() - 2 * verticalGap; // leave a gap on the top and bottom
        // height of each entry; 9 stored recipes
        final int entryHeight = (int) (usableHeight / recipesPerColumn);
        // leave 0.25-th of a stack height gap between each entry
        final float scale = entryHeight / (stackBaseHeight * 1.25f);
        final int stackScaledSize = (int) (stackBaseHeight * scale);
        // Leave a small gap from the rendered stack to the gui's left edge. The +12 is some space for the recipe's number text.
        final int columnOffsetCount = (recipeCount / recipesPerColumn);

        final int recipeDimensions = (int) Math.ceil(Math.sqrt(recipe.getRecipeLength()));
        final float x = AccessorUtils.getGuiLeft(gui) - (columnOffsetCount + 0.2f) * stackScaledSize - (columnOffsetCount - 1) * scale * 20 - (stackBaseHeight + 1) * (recipeDimensions + 1) * scale;
        final float y = window.getScaledHeight() / 2 - (stackBaseHeight + 1) * recipeDimensions * scale * 0.5f;

        return new RecipeLocation(x, y, scale, stackBaseHeight, recipeDimensions);
    }

    private ItemStack getHoveredRecipeIngredient(int mouseX, int mouseY, RecipePattern recipe, int recipeCount, ContainerScreen<?> gui, MinecraftClient mc)
    {
        RecipeLocation location = this.getRecipeLocation(recipe, recipeCount, gui, mc);
        final float stackWidth = location.scale * location.stackBaseHeight;

        for (int i = 0, row = 0; row < location.recipeDimensions; row++)
        {
            for (int col = 0; col < location.recipeDimensions; col++, i++)
            {
                float xOff = col * ((location.stackBaseHeight + 1) * location.scale);
                float yOff = row * ((location.stackBaseHeight + 1) * location.scale);
                float xStart = location.x + xOff;
                float yStart = location.y + yOff;

                if (mouseX >= xStart && mouseX <= xStart + stackWidth && mouseY >= yStart && mouseY <= yStart + stackWidth)
                {
                    return recipe.getRecipeItems()[i];
                }
            }
        }

        return ItemStack.EMPTY;
    }

    private static class RecipeLocation
    {
        public final float x;
        public final float y;
        public final float scale;
        public final int stackBaseHeight;
        public final int recipeDimensions;

        public RecipeLocation(float x, float y, float scale, int stackBaseHeight, int recipeDimensions)
        {
            this.x = x;
            this.y = y;
            this.scale = scale;
            this.stackBaseHeight = stackBaseHeight;
            this.recipeDimensions = recipeDimensions;
        }
    }

    private void renderStackAt(ItemStack stack, float x, float y, boolean border, float scale, int stackBaseHeight, MinecraftClient mc)
    {
        GlStateManager.pushMatrix();
        GlStateManager.translatef(x, y, 0);
        GlStateManager.scalef(scale, scale, 1);
        GlStateManager.disableLighting();
        final int w = stackBaseHeight;

        if (border)
        {
            // Draw a light/white border around the stack
            DrawableHelper.fill(    0,     0, w, 1, 0xFFFFFFFF);
            DrawableHelper.fill(    0,     0, 1, w, 0xFFFFFFFF);
            DrawableHelper.fill(w - 1,     0, w, w, 0xFFFFFFFF);
            DrawableHelper.fill(    0, w - 1, w, w, 0xFFFFFFFF);

            DrawableHelper.fill(1, 1, w - 1, w - 1, 0x20FFFFFF); // light background for the item
        }
        else
        {
            DrawableHelper.fill(0, 0, w, w, 0x20FFFFFF); // light background for the item
        }

        if (InventoryUtils.isStackEmpty(stack) == false)
        {
            enableGUIStandardItemLighting(scale);

            stack = stack.copy();
            InventoryUtils.setStackSize(stack, 1);
            mc.getItemRenderer().zOffset += 100;
            mc.getItemRenderer().renderGuiItem(mc.player, stack, 0, 0);
            //mc.getRenderItem().renderItemOverlayIntoGUI(mc.textRenderer, stack, 0, 0, null);
            mc.getItemRenderer().zOffset -= 100;
        }

        GlStateManager.disableBlend();
        GuiLighting.disable();
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
        GlStateManager.light(16384, 4611, GuiLighting.singletonBuffer((float) LIGHT0_POS.x, (float) LIGHT0_POS.y, (float) LIGHT0_POS.z, 0.0f));

        float lightStrength = 0.3F * scale;
        GlStateManager.light(16384, 4609, GuiLighting.singletonBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.light(16384, 4608, GuiLighting.singletonBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.light(16384, 4610, GuiLighting.singletonBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.light(16385, 4611, GuiLighting.singletonBuffer((float) LIGHT1_POS.x, (float) LIGHT1_POS.y, (float) LIGHT1_POS.z, 0.0f));
        GlStateManager.light(16385, 4609, GuiLighting.singletonBuffer(lightStrength, lightStrength, lightStrength, 1.0F));
        GlStateManager.light(16385, 4608, GuiLighting.singletonBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.light(16385, 4610, GuiLighting.singletonBuffer(0.0F, 0.0F, 0.0F, 1.0F));
        GlStateManager.shadeModel(7424);

        float ambientLightStrength = 0.4F;
        GlStateManager.lightModel(2899, GuiLighting.singletonBuffer(ambientLightStrength, ambientLightStrength, ambientLightStrength, 1.0F));
    }

    private void renderStackToolTip(int x, int y, ItemStack stack, ContainerScreen<?> gui, MinecraftClient mc)
    {
        List<TextComponent> list = stack.getTooltipText(mc.player, mc.options.advancedItemTooltips ? TooltipContext.Default.ADVANCED : TooltipContext.Default.NORMAL);

        for (int i = 0; i < list.size(); ++i)
        {
            if (i == 0)
            {
                list.set(i, new StringTextComponent(stack.getRarity().formatting + list.get(i).getString()));
            }
            else
            {
                list.set(i, new StringTextComponent(TextFormat.GRAY + list.get(i).getString()));
            }
        }

        TextRenderer font = mc.textRenderer;

        drawHoveringText(stack, list, x, y, gui.width, gui.height, -1, font);
    }

    private static void drawHoveringText(@Nonnull final ItemStack stack, List<TextComponent> components, int mouseX, int mouseY, int screenWidth, int screenHeight, int maxTextWidth, TextRenderer font)
    {
        List<String> textLines = new ArrayList<>();

        if (!components.isEmpty())
        {
            GlStateManager.disableRescaleNormal();
            GuiLighting.disable();
            GlStateManager.disableLighting();
            GlStateManager.disableDepthTest();
            int tooltipTextWidth = 0;

            for (TextComponent entry : components)
            {
                String text = entry.getString();
                int textLineWidth = font.getStringWidth(text);

                if (textLineWidth > tooltipTextWidth)
                {
                    tooltipTextWidth = textLineWidth;
                }

                textLines.add(text);
            }

            boolean needsWrap = false;

            int titleLinesCount = 1;
            int tooltipX = mouseX + 12;

            if (tooltipX + tooltipTextWidth + 4 > screenWidth)
            {
                tooltipX = mouseX - 16 - tooltipTextWidth;
                if (tooltipX < 4) // if the tooltip doesn't fit on the screen
                {
                    if (mouseX > screenWidth / 2)
                    {
                        tooltipTextWidth = mouseX - 12 - 8;
                    }
                    else
                    {
                        tooltipTextWidth = screenWidth - 16 - mouseX;
                    }
                    needsWrap = true;
                }
            }

            if (maxTextWidth > 0 && tooltipTextWidth > maxTextWidth)
            {
                tooltipTextWidth = maxTextWidth;
                needsWrap = true;
            }

            if (needsWrap)
            {
                int wrappedTooltipWidth = 0;
                List<String> wrappedTextLines = new ArrayList<>();

                for (int i = 0; i < textLines.size(); i++)
                {
                    String textLine = textLines.get(i);
                    List<String> wrappedLine = font.wrapStringToWidthAsList(textLine, tooltipTextWidth);

                    if (i == 0)
                    {
                        titleLinesCount = wrappedLine.size();
                    }

                    for (String line : wrappedLine)
                    {
                        int lineWidth = font.getStringWidth(line);
                        if (lineWidth > wrappedTooltipWidth)
                        {
                            wrappedTooltipWidth = lineWidth;
                        }
                        wrappedTextLines.add(line);
                    }
                }
                tooltipTextWidth = wrappedTooltipWidth;
                textLines = wrappedTextLines;

                if (mouseX > screenWidth / 2)
                {
                    tooltipX = mouseX - 16 - tooltipTextWidth;
                }
                else
                {
                    tooltipX = mouseX + 12;
                }
            }

            int tooltipY = mouseY - 12;
            int tooltipHeight = 8;

            if (textLines.size() > 1)
            {
                tooltipHeight += (textLines.size() - 1) * 10;
                if (textLines.size() > titleLinesCount) {
                    tooltipHeight += 2; // gap between title lines and next lines
                }
            }

            if (tooltipY < 4)
            {
                tooltipY = 4;
            }
            else if (tooltipY + tooltipHeight + 4 > screenHeight)
            {
                tooltipY = screenHeight - tooltipHeight - 4;
            }

            final int zLevel = 300;
            final int backgroundColor = 0xF0100010;
            drawGradientRect(zLevel, tooltipX - 3, tooltipY - 4, tooltipX + tooltipTextWidth + 3, tooltipY - 3, backgroundColor, backgroundColor);
            drawGradientRect(zLevel, tooltipX - 3, tooltipY + tooltipHeight + 3, tooltipX + tooltipTextWidth + 3, tooltipY + tooltipHeight + 4, backgroundColor, backgroundColor);
            drawGradientRect(zLevel, tooltipX - 3, tooltipY - 3, tooltipX + tooltipTextWidth + 3, tooltipY + tooltipHeight + 3, backgroundColor, backgroundColor);
            drawGradientRect(zLevel, tooltipX - 4, tooltipY - 3, tooltipX - 3, tooltipY + tooltipHeight + 3, backgroundColor, backgroundColor);
            drawGradientRect(zLevel, tooltipX + tooltipTextWidth + 3, tooltipY - 3, tooltipX + tooltipTextWidth + 4, tooltipY + tooltipHeight + 3, backgroundColor, backgroundColor);
            final int borderColorStart = 0x505000FF;
            final int borderColorEnd = (borderColorStart & 0xFEFEFE) >> 1 | borderColorStart & 0xFF000000;
            drawGradientRect(zLevel, tooltipX - 3, tooltipY - 3 + 1, tooltipX - 3 + 1, tooltipY + tooltipHeight + 3 - 1, borderColorStart, borderColorEnd);
            drawGradientRect(zLevel, tooltipX + tooltipTextWidth + 2, tooltipY - 3 + 1, tooltipX + tooltipTextWidth + 3, tooltipY + tooltipHeight + 3 - 1, borderColorStart, borderColorEnd);
            drawGradientRect(zLevel, tooltipX - 3, tooltipY - 3, tooltipX + tooltipTextWidth + 3, tooltipY - 3 + 1, borderColorStart, borderColorStart);
            drawGradientRect(zLevel, tooltipX - 3, tooltipY + tooltipHeight + 2, tooltipX + tooltipTextWidth + 3, tooltipY + tooltipHeight + 3, borderColorEnd, borderColorEnd);

            for (int lineNumber = 0; lineNumber < textLines.size(); ++lineNumber)
            {
                String line = textLines.get(lineNumber);
                font.drawWithShadow(line, (float)tooltipX, (float)tooltipY, -1);

                if (lineNumber + 1 == titleLinesCount)
                {
                    tooltipY += 2;
                }

                tooltipY += 10;
            }

            GlStateManager.enableLighting();
            GlStateManager.enableDepthTest();
            GuiLighting.enable();
            GlStateManager.enableRescaleNormal();
        }
    }

    public static void drawGradientRect(int zLevel, int left, int top, int right, int bottom, int startColor, int endColor)
    {
        float startAlpha = (float)(startColor >> 24 & 255) / 255.0F;
        float startRed   = (float)(startColor >> 16 & 255) / 255.0F;
        float startGreen = (float)(startColor >> 8 & 255) / 255.0F;
        float startBlue  = (float)(startColor & 255) / 255.0F;
        float endAlpha   = (float)(endColor >> 24 & 255) / 255.0F;
        float endRed     = (float)(endColor >> 16 & 255) / 255.0F;
        float endGreen   = (float)(endColor >> 8 & 255) / 255.0F;
        float endBlue    = (float)(endColor & 255) / 255.0F;

        GlStateManager.disableTexture();
        GlStateManager.enableBlend();
        GlStateManager.disableAlphaTest();
        GlStateManager.blendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);
        GlStateManager.shadeModel(GL11.GL_SMOOTH);

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBufferBuilder();
        buffer.begin(GL11.GL_QUADS, VertexFormats.POSITION_COLOR);
        buffer.vertex(right, top,    zLevel).color(startRed, startGreen, startBlue, startAlpha).next();
        buffer.vertex(left,  top,    zLevel).color(startRed, startGreen, startBlue, startAlpha).next();
        buffer.vertex(left,  bottom, zLevel).color(endRed,   endGreen,   endBlue,   endAlpha).next();
        buffer.vertex(right, bottom, zLevel).color(endRed,   endGreen,   endBlue,   endAlpha).next();
        tessellator.draw();

        GlStateManager.shadeModel(GL11.GL_FLAT);
        GlStateManager.disableBlend();
        GlStateManager.enableAlphaTest();
        GlStateManager.enableTexture();
    }
}
