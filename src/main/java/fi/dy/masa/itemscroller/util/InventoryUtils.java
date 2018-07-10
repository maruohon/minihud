package fi.dy.masa.itemscroller.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.lwjgl.input.Mouse;
import fi.dy.masa.itemscroller.LiteModItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Configs.Toggles;
import fi.dy.masa.itemscroller.event.InputEventHandler.MoveAmount;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.CraftingHandler.SlotRange;
import fi.dy.masa.itemscroller.recipes.CraftingRecipe;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.InventoryPlayer;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.InventoryCraftResult;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotMerchantResult;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.CraftingManager;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.util.ResourceLocation;
import net.minecraft.village.MerchantRecipe;
import net.minecraft.village.MerchantRecipeList;
import net.minecraft.world.World;

public class InventoryUtils
{
    public static void onSlotChangedCraftingGrid(World world, EntityPlayer player, InventoryCrafting inventoryCrafting, InventoryCraftResult inventoryCraftResult)
    {
        if (Configs.Toggles.CLIENT_CRAFTING_FIX.getValue() &&
            world.isRemote && player instanceof EntityPlayerSP)
        {
            ItemStack stack = ItemStack.EMPTY;
            IRecipe recipe = CraftingManager.findMatchingRecipe(inventoryCrafting, world);

            if (recipe != null &&
                    (recipe.isDynamic() ||
                     world.getGameRules().getBoolean("doLimitedCrafting") == false ||
                     ((EntityPlayerSP) player).getRecipeBook().isUnlocked(recipe))
            )
            {
                inventoryCraftResult.setRecipeUsed(recipe);
                stack = recipe.getCraftingResult(inventoryCrafting);
            }

            inventoryCraftResult.setInventorySlotContents(0, stack);
        }
    }

    public static String getStackString(ItemStack stack)
    {
        if (isStackEmpty(stack) == false)
        {
            ResourceLocation rl = Item.REGISTRY.getNameForObject(stack.getItem());

            return String.format("[%s @ %d - display: %s - NBT: %s] (%s)",
                    rl != null ? rl.toString() : "null", stack.getMetadata(), stack.getDisplayName(),
                    stack.getTagCompound() != null ? stack.getTagCompound().toString() : "<no NBT>",
                    stack.toString());
        }

        return "<empty>";
    }

    public static boolean isValidSlot(Slot slot, GuiContainer gui, boolean requireItems)
    {
        return gui.inventorySlots != null && gui.inventorySlots.inventorySlots != null &&
                slot != null && gui.inventorySlots.inventorySlots.contains(slot) &&
                (requireItems == false || slot.getHasStack()) &&
                Configs.SLOT_BLACKLIST.contains(slot.getClass().getName()) == false;
    }

    public static boolean isCraftingSlot(GuiContainer gui, Slot slot)
    {
        return slot != null && CraftingHandler.getCraftingGridSlots(gui, slot) != null;
    }

    /**
     * Checks if there are slots belonging to another inventory on screen above the given slot
     */
    public static boolean inventoryExistsAbove(Slot slot, Container container)
    {
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.yPos < slot.yPos && areSlotsInSameInventory(slot, slotTmp) == false)
            {
                return true;
            }
        }

        return false;
    }

    public static boolean canShiftPlaceItems(GuiContainer gui)
    {
        if (GuiScreen.isShiftKeyDown() == false || Mouse.getEventButton() != 0)
        {
            return false;
        }

        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getMinecraft();
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        // The target slot needs to be an empty, valid slot, and there needs to be items in the cursor
        return slot != null && isStackEmpty(stackCursor) == false && isValidSlot(slot, gui, false) &&
               slot.getHasStack() == false && slot.isItemValid(stackCursor);
    }

    public static boolean tryMoveItems(GuiContainer gui, RecipeStorage recipes, boolean scrollingUp)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getMinecraft();

        // We require an empty cursor
        if (slot == null || isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            return false;
        }

        // Villager handling only happens when scrolling over the trade output slot
        boolean villagerHandling = Configs.Toggles.SCROLL_VILLAGER.getValue() && gui instanceof GuiMerchant && slot instanceof SlotMerchantResult;
        boolean craftingHandling = Configs.Toggles.SCROLL_CRAFT.getValue() && isCraftingSlot(gui, slot);
        boolean isCtrlDown = GuiContainer.isCtrlKeyDown();
        boolean isShiftDown = GuiContainer.isShiftKeyDown();
        boolean moveToOtherInventory = scrollingUp;

        if (Configs.Toggles.SLOT_POSITION_AWARE_SCROLL_DIRECTION.getValue())
        {
            boolean above = inventoryExistsAbove(slot, gui.inventorySlots);
            // so basically: (above && scrollingUp) || (above == false && scrollingUp == false)
            moveToOtherInventory = above == scrollingUp;
        }

        if ((Configs.Toggles.REVERSE_SCROLL_DIRECTION_SINGLE.getValue() && isShiftDown == false) ||
            (Configs.Toggles.REVERSE_SCROLL_DIRECTION_STACKS.getValue() && isShiftDown))
        {
            moveToOtherInventory = ! moveToOtherInventory;
        }

        // Check that the slot is valid, (don't require items in case of the villager output slot or a crafting slot)
        if (isValidSlot(slot, gui, villagerHandling || craftingHandling ? false : true) == false)
        {
            return false;
        }

        if (craftingHandling)
        {
            return tryMoveItemsCrafting(recipes, slot, gui, moveToOtherInventory, isShiftDown, isCtrlDown);
        }

        if (villagerHandling)
        {
            return tryMoveItemsVillager((GuiMerchant) gui, slot, moveToOtherInventory, isShiftDown);
        }

        if ((Configs.Toggles.SCROLL_SINGLE.getValue() == false && isShiftDown == false && isCtrlDown == false) ||
            (Configs.Toggles.SCROLL_STACKS.getValue() == false && isShiftDown && isCtrlDown == false) ||
            (Configs.Toggles.SCROLL_MATCHING.getValue() == false && isShiftDown == false && isCtrlDown) ||
            (Configs.Toggles.SCROLL_EVERYTHING.getValue() == false && isShiftDown && isCtrlDown))
        {
            return false;
        }

        if (isShiftDown)
        {
            // Ctrl + Shift + scroll: move everything
            if (isCtrlDown)
            {
                tryMoveStacks(slot, gui, false, moveToOtherInventory, false);
            }
            // Shift + scroll: move one matching stack
            else
            {
                tryMoveStacks(slot, gui, true, moveToOtherInventory, true);
            }

            return true;
        }
        // Ctrl + scroll: Move all matching stacks
        else if (isCtrlDown)
        {
            tryMoveStacks(slot, gui, true, moveToOtherInventory, false);
            return true;
        }
        // No Ctrl or Shift
        else
        {
            ItemStack stack = slot.getStack();

            // Scrolling items from this slot/inventory into the other inventory
            if (moveToOtherInventory)
            {
                return tryMoveSingleItemToOtherInventory(slot, gui);
            }
            // Scrolling items from the other inventory into this slot/inventory
            else if (getStackSize(stack) < slot.getItemStackLimit(stack))
            {
                return tryMoveSingleItemToThisInventory(slot, gui);
            }
        }

        return false;
    }

    public static void dropStacks(GuiContainer gui, ItemStack stackReference, Slot slotReference, boolean sameInventory)
    {
        if (slotReference != null && isStackEmpty(stackReference) == false)
        {
            Container container = gui.inventorySlots;
            stackReference = stackReference.copy();

            for (Slot slot : container.inventorySlots)
            {
                // If this slot is in the same inventory that the items were picked up to the cursor from
                // and the stack is identical to the one in the cursor, then this stack will get dropped.
                if (areSlotsInSameInventory(slot, slotReference) == sameInventory && areStacksEqual(slot.getStack(), stackReference))
                {
                    // Drop the stack
                    dropStack(gui, slot.slotNumber);
                }
            }
        }
    }

    public static boolean tryMoveItemsVillager(GuiMerchant gui, Slot slot, boolean moveToOtherInventory, boolean fullStacks)
    {
        if (fullStacks)
        {
            // Try to fill the merchant's buy slots from the player inventory
            if (moveToOtherInventory == false)
            {
                tryMoveItemsToMerchantBuySlots(gui, true);
            }
            // Move items from sell slot to player inventory
            else if (slot.getHasStack())
            {
                tryMoveStacks(slot, gui, true, true, true);
            }
            // Scrolling over an empty output slot, clear the buy slots
            else
            {
                tryMoveStacks(slot, gui, false, true, false);
            }
        }
        else
        {
            // Scrolling items from player inventory into merchant buy slots
            if (moveToOtherInventory == false)
            {
                tryMoveItemsToMerchantBuySlots(gui, false);
            }
            // Scrolling items from this slot/inventory into the other inventory
            else if (slot.getHasStack())
            {
                moveOneSetOfItemsFromSlotToOtherInventory(gui, slot);
            }
        }

        return false;
    }

    public static boolean tryMoveSingleItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        ItemStack stackOrig = slot.getStack();
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getMinecraft();

        if (isStackEmpty(mc.player.inventory.getItemStack()) == false || slot.canTakeStack(mc.player) == false ||
            (getStackSize(stackOrig) > 1 && slot.isItemValid(stackOrig) == false))
        {
            return false;
        }

        // Can take all the items to the cursor at once, use a shift-click method to move one item from the slot
        if (getStackSize(stackOrig) <= stackOrig.getMaxStackSize())
        {
            return clickSlotsToMoveSingleItemByShiftClick(gui, slot.slotNumber);
        }

        ItemStack stack = stackOrig.copy();
        setStackSize(stack, 1);

        ItemStack[] originalStacks = getOriginalStacks(container);

        // Try to move the temporary single-item stack via the shift-click handler method
        slot.putStack(stack);
        container.transferStackInSlot(mc.player, slot.slotNumber);

        // Successfully moved the item somewhere, now we want to check where it went
        if (slot.getHasStack() == false)
        {
            int targetSlot = getTargetSlot(container, originalStacks);

            // Found where the item went
            if (targetSlot >= 0)
            {
                // Remove the dummy item from the target slot (on the client side)
                container.inventorySlots.get(targetSlot).decrStackSize(1);

                // Restore the original stack to the slot under the cursor (on the client side)
                restoreOriginalStacks(container, originalStacks);

                // Do the slot clicks to actually move the items (on the server side)
                return clickSlotsToMoveSingleItem(gui, slot.slotNumber, targetSlot);
            }
        }

        // Restore the original stack to the slot under the cursor (on the client side)
        slot.putStack(stackOrig);

        return false;
    }

    public static boolean tryMoveAllButOneItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        Minecraft mc = Minecraft.getMinecraft();
        EntityPlayer player = mc.player;
        ItemStack stackOrig = slot.getStack().copy();

        if (getStackSize(stackOrig) == 1 || getStackSize(stackOrig) > stackOrig.getMaxStackSize() ||
            slot.canTakeStack(player) == false || slot.isItemValid(stackOrig) == false)
        {
            return true;
        }

        // Take half of the items from the original slot to the cursor
        rightClickSlot(gui, slot.slotNumber);

        ItemStack stackInCursor = player.inventory.getItemStack();
        if (isStackEmpty(stackInCursor))
        {
            return false;
        }

        int stackInCursorSizeOrig = getStackSize(stackInCursor);
        int tempSlotNum = -1;

        // Find some other slot where to store one of the items temporarily
        for (Slot slotTmp : gui.inventorySlots.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot, true) &&
                slotTmp.isItemValid(stackInCursor) &&
                slotTmp.canTakeStack(player))
            {
                ItemStack stackInSlot = slotTmp.getStack();

                if (isStackEmpty(stackInSlot) || areStacksEqual(stackInSlot, stackInCursor))
                {
                    // Try to put one item into the temporary slot
                    rightClickSlot(gui, slotTmp.slotNumber);

                    stackInCursor = player.inventory.getItemStack();

                    // Successfully stored one item
                    if (isStackEmpty(stackInCursor) || getStackSize(stackInCursor) < stackInCursorSizeOrig)
                    {
                        tempSlotNum = slotTmp.slotNumber;
                        break;
                    }
                }
            }
        }

        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            // Return the rest of the items into the original slot
            leftClickSlot(gui, slot.slotNumber);
        }

        // Successfully stored one item in a temporary slot
        if (tempSlotNum != -1)
        {
            // Shift click the stack from the original slot
            shiftClickSlot(gui, slot.slotNumber);

            // Take half a stack from the temporary slot
            rightClickSlot(gui, tempSlotNum);

            // Return one item into the original slot
            rightClickSlot(gui, slot.slotNumber);

            // Return the rest of the items to the temporary slot, if any
            if (isStackEmpty(player.inventory.getItemStack()) == false)
            {
                leftClickSlot(gui, tempSlotNum);
            }

            return true;
        }
        // No temporary slot found, try to move the stack manually
        else
        {
            boolean treatHotbarAsDifferent = gui.getClass() == GuiInventory.class;
            List<Integer> slots = getSlotNumbersOfEmptySlots(gui.inventorySlots, slot, false, treatHotbarAsDifferent, false);

            if (slots.isEmpty())
            {
                slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, false, slot.getStack(), true, treatHotbarAsDifferent, false);
            }

            if (slots.isEmpty() == false)
            {
                // Take the stack
                leftClickSlot(gui, slot.slotNumber);

                // Return one item
                rightClickSlot(gui, slot.slotNumber);

                // Try to place the stack in the cursor to any valid empty or matching slots in a different inventory
                for (int slotNum : slots)
                {
                    Slot slotTmp = gui.inventorySlots.getSlot(slotNum);
                    stackInCursor = player.inventory.getItemStack();

                    if (isStackEmpty(stackInCursor))
                    {
                        return true;
                    }

                    if (slotTmp.isItemValid(stackInCursor))
                    {
                        leftClickSlot(gui, slotNum);
                    }
                }

                // Items left, return them
                if (isStackEmpty(stackInCursor) == false)
                {
                    leftClickSlot(gui, slot.slotNumber);
                }
            }
        }

        return false;
    }

    public static boolean tryMoveSingleItemToThisInventory(Slot slot, GuiContainer gui)
    {
        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();
        Minecraft mc = Minecraft.getMinecraft();

        if (slot.isItemValid(stackOrig) == false)
        {
            return false;
        }

        for (int slotNum = container.inventorySlots.size() - 1; slotNum >= 0; slotNum--)
        {
            Slot slotTmp = container.inventorySlots.get(slotNum);
            ItemStack stackTmp = slotTmp.getStack();

            if (areSlotsInSameInventory(slotTmp, slot) == false &&
                isStackEmpty(stackTmp) == false && slotTmp.canTakeStack(mc.player) &&
                (getStackSize(stackTmp) == 1 || slotTmp.isItemValid(stackTmp)))
            {
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    return clickSlotsToMoveSingleItem(gui, slotTmp.slotNumber, slot.slotNumber);
                }
            }
        }

        // If we weren't able to move any items from another inventory, then try to move items
        // within the same inventory (mostly between the hotbar and the player inventory)
        /*
        for (Slot slotTmp : container.inventorySlots)
        {
            ItemStack stackTmp = slotTmp.getStack();

            if (slotTmp.slotNumber != slot.slotNumber &&
                isStackEmpty(stackTmp) == false && slotTmp.canTakeStack(gui.mc.player) &&
                (getStackSize(stackTmp) == 1 || slotTmp.isItemValid(stackTmp)))
            {
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    return this.clickSlotsToMoveSingleItem(gui, slotTmp.slotNumber, slot.slotNumber);
                }
            }
        }
        */

        return false;
    }

    public static void tryMoveStacks(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        tryMoveStacks(slot.getStack(), slot, gui, matchingOnly, toOtherInventory, firstOnly);
    }

    private static void tryMoveStacks(ItemStack stackReference, Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        Container container = gui.inventorySlots;
        final int maxSlot = container.inventorySlots.size() - 1;

        for (int i = maxSlot; i >= 0; i--)
        {
            Slot slotTmp = container.inventorySlots.get(i);

            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot) == toOtherInventory && slotTmp.getHasStack() &&
                (matchingOnly == false || areStacksEqual(stackReference, slotTmp.getStack())))
            {
                boolean success = shiftClickSlotWithCheck(gui, slotTmp.slotNumber);

                // Failed to shift-click items, try a manual method
                if (success == false && Configs.Toggles.SCROLL_STACKS_FALLBACK.getValue())
                {
                    clickSlotsToMoveItemsFromSlot(slotTmp, gui, toOtherInventory);
                }

                if (firstOnly)
                {
                    return;
                }
            }
        }

        // If moving to the other inventory, then move the hovered slot's stack last
        if (toOtherInventory && shiftClickSlotWithCheck(gui, slot.slotNumber) == false)
        {
            clickSlotsToMoveItemsFromSlot(slot, gui, toOtherInventory);
        }
    }

    private static void tryMoveItemsToMerchantBuySlots(GuiMerchant gui, boolean fillStacks)
    {
        Minecraft mc = Minecraft.getMinecraft();
        MerchantRecipeList list = gui.getMerchant().getRecipes(mc.player);
        int index = AccessorUtils.getSelectedMerchantRecipe(gui);

        if (list == null || list.size() <= index)
        {
            return;
        }

        MerchantRecipe recipe = list.get(index);
        if (recipe == null)
        {
            return;
        }

        ItemStack buy1 = recipe.getItemToBuy();
        ItemStack buy2 = recipe.getSecondItemToBuy();

        if (isStackEmpty(buy1) == false)
        {
            fillBuySlot(gui, 0, buy1, fillStacks);
        }

        if (isStackEmpty(buy2) == false)
        {
            fillBuySlot(gui, 1, buy2, fillStacks);
        }
    }

    public static void fillBuySlot(GuiContainer gui, int slotNum, ItemStack buyStack, boolean fillStacks)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);
        ItemStack existingStack = slot.getStack();
        Minecraft mc = Minecraft.getMinecraft();

        // If there are items not matching the merchant recipe, move them out first
        if (isStackEmpty(existingStack) == false && areStacksEqual(buyStack, existingStack) == false)
        {
            shiftClickSlot(gui, slotNum);
        }

        existingStack = slot.getStack();

        if (isStackEmpty(existingStack) || areStacksEqual(buyStack, existingStack))
        {
            moveItemsFromInventory(gui, slotNum, mc.player.inventory, buyStack, fillStacks);
        }
    }

    public static void tryMoveItemsToFirstCraftingGrid(CraftingRecipe recipe, GuiContainer gui, boolean fillStacks)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            tryMoveItemsToCraftingGridSlots(recipe, craftingOutputSlot, gui, fillStacks);
        }
    }

    public static void loadRecipeItemsToGridForOutputSlotUnderMouse(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        loadRecipeItemsToGridForOutputSlot(recipe, gui, slot);
    }

    public static void loadRecipeItemsToGridForOutputSlot(CraftingRecipe recipe, GuiContainer gui, Slot outputSlot)
    {
        if (outputSlot != null && isCraftingSlot(gui, outputSlot) && isStackEmpty(recipe.getResult()) == false)
        {
            tryMoveItemsToCraftingGridSlots(recipe, outputSlot, gui, false);
        }
    }

    private static boolean tryMoveItemsCrafting(RecipeStorage recipes, Slot slot, GuiContainer gui,
            boolean moveToOtherInventory, boolean isShiftDown, boolean isCtrlDown)
    {
        CraftingRecipe recipe = recipes.getSelectedRecipe();

        if (isShiftDown)
        {
            // Try to fill the crafting grid
            if (moveToOtherInventory == false)
            {
                if (isStackEmpty(recipes.getSelectedRecipe().getResult()) == false)
                {
                    tryMoveItemsToCraftingGridSlots(recipe, slot, gui, true);
                }
            }
            // Move items from the crafting output slot
            else if (slot.getHasStack())
            {
                if (isCtrlDown)
                {
                    craftAsManyItemsAsPossible(recipe, slot, gui);
                }
                else
                {
                    shiftClickSlot(gui, slot.slotNumber);
                }
            }
            // Scrolling over an empty crafting output slot, clear the crafting grid
            else
            {
                SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

                if (range != null)
                {
                    for (int i = 0, s = range.getFirst(); i < range.getSlotCount(); i++, s++)
                    {
                        shiftClickSlot(gui, s);
                    }
                }
            }
        }
        else
        {
            // Scrolling items from player inventory into crafting grid slots
            if (moveToOtherInventory == false)
            {
                if (isStackEmpty(recipes.getSelectedRecipe().getResult()) == false)
                {
                    tryMoveItemsToCraftingGridSlots(recipe, slot, gui, false);
                }
            }
            // Scrolling items from this crafting slot into the other inventory
            else if (slot.getHasStack())
            {
                moveOneSetOfItemsFromSlotToOtherInventory(gui, slot);
            }
            // Scrolling over an empty crafting output slot, clear the crafting grid
            else
            {
                SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

                if (range != null)
                {
                    for (int i = 0, s = range.getFirst(); i < range.getSlotCount(); i++, s++)
                    {
                        shiftClickSlot(gui, s);
                    }
                }
            }
        }

        return false;
    }

    private static void craftAsManyItemsAsPossible(CraftingRecipe recipe, Slot slot, GuiContainer gui)
    {
        ItemStack result = recipe.getResult();
        int failSafe = 1024;

        while (failSafe > 0 && slot.getHasStack() && areStacksEqual(slot.getStack(), result))
        {
            shiftClickSlot(gui, slot.slotNumber);

            // Ran out of some or all ingredients for the recipe
            if (slot.getHasStack() == false || areStacksEqual(slot.getStack(), result) == false)
            {
                tryMoveItemsToCraftingGridSlots(recipe, slot, gui, true);
            }
            // No change in the result slot after shift clicking, let's assume the craft failed and stop here
            else
            {
                break;
            }

            failSafe--;
        }
    }

    public static void clearFirstCraftingGridOfItems(CraftingRecipe recipe, GuiContainer gui, boolean clearNonMatchingOnly)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, craftingOutputSlot);
            clearCraftingGridOfItems(recipe, gui, range, clearNonMatchingOnly);
        }
    }

    public static void clearFirstCraftingGridOfAllItems(GuiContainer gui)
    {
        Slot craftingOutputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (craftingOutputSlot != null)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, craftingOutputSlot);
            clearCraftingGridOfAllItems(gui, range);
        }
    }

    private static boolean clearCraftingGridOfItems(CraftingRecipe recipe, GuiContainer gui, SlotRange range, boolean clearNonMatchingOnly)
    {
        final int invSlots = gui.inventorySlots.inventorySlots.size();
        final int rangeSlots = range.getSlotCount();
        final int recipeSize = recipe.getRecipeLength();
        final int slotCount = Math.min(rangeSlots, recipeSize);

        for (int i = 0, slotNum = range.getFirst(); i < slotCount && slotNum < invSlots; i++, slotNum++)
        {
            Slot slotTmp = gui.inventorySlots.getSlot(slotNum);

            if (slotTmp != null && slotTmp.getHasStack() &&
                (clearNonMatchingOnly == false || areStacksEqual(recipe.getRecipeItems()[i], slotTmp.getStack()) == false))
            {
                shiftClickSlot(gui, slotNum);

                // Failed to clear the slot
                if (slotTmp.getHasStack())
                {
                    return false;
                }
            }
        }

        return true;
    }

    private static boolean clearCraftingGridOfAllItems(GuiContainer gui, SlotRange range)
    {
        final int invSlots = gui.inventorySlots.inventorySlots.size();
        final int rangeSlots = range.getSlotCount();
        boolean clearedAll = true;

        for (int i = 0, slotNum = range.getFirst(); i < rangeSlots && slotNum < invSlots; i++, slotNum++)
        {
            Slot slotTmp = gui.inventorySlots.getSlot(slotNum);

            if (slotTmp != null && slotTmp.getHasStack())
            {
                shiftClickSlot(gui, slotNum);

                // Failed to clear the slot
                if (slotTmp.getHasStack())
                {
                    clearedAll = false;
                }
            }
        }

        return clearedAll;
    }

    private static boolean tryMoveItemsToCraftingGridSlots(CraftingRecipe recipe, Slot slot, GuiContainer gui, boolean fillStacks)
    {
        Container container = gui.inventorySlots;
        int numSlots = container.inventorySlots.size();
        SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

        // Check that the slot range is valid and that the recipe can fit into this type of crafting grid
        if (range != null && range.getLast() < numSlots && recipe.getRecipeLength() <= range.getSlotCount())
        {
            // Clear non-matching items from the grid first
            if (clearCraftingGridOfItems(recipe, gui, range, true) == false)
            {
                return false;
            }

            // This slot is used to check that we get items from a DIFFERENT inventory than where this slot is in
            Slot slotGridFirst = container.getSlot(range.getFirst());
            Map<ItemType, List<Integer>> ingredientSlots = ItemType.getSlotsPerItem(recipe.getRecipeItems());

            for (Map.Entry<ItemType, List<Integer>> entry : ingredientSlots.entrySet())
            {
                ItemStack ingredientReference = entry.getKey().getStack();
                List<Integer> recipeSlots = entry.getValue();
                List<Integer> targetSlots = new ArrayList<Integer>();

                // Get the actual target slot numbers based on the grid's start and the relative positions inside the grid
                for (int s : recipeSlots)
                {
                    targetSlots.add(s + range.getFirst());
                }

                if (fillStacks)
                {
                    fillCraftingGrid(gui, slotGridFirst, ingredientReference, targetSlots);
                }
                else
                {
                    moveOneRecipeItemIntoCraftingGrid(gui, slotGridFirst, ingredientReference, targetSlots);
                }
            }
        }

        return false;
    }

    public static void fillCraftingGrid(GuiContainer gui, Slot slotGridFirst, ItemStack ingredientReference, List<Integer> targetSlots)
    {
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getMinecraft();
        EntityPlayer player = mc.player;
        int slotNum = -1;
        int slotReturn = -1;
        int sizeOrig = 0;

        if (isStackEmpty(ingredientReference))
        {
            return;
        }

        while (true)
        {
            slotNum = getSlotNumberOfLargestMatchingStackFromDifferentInventory(container, slotGridFirst, ingredientReference);

            // Didn't find ingredient items
            if (slotNum < 0)
            {
                break;
            }

            if (slotReturn == -1)
            {
                slotReturn = slotNum;
            }

            // Pick up the ingredient stack from the found slot
            leftClickSlot(gui, slotNum);

            ItemStack stackCursor = player.inventory.getItemStack();

            // Successfully picked up ingredient items
            if (areStacksEqual(ingredientReference, stackCursor))
            {
                sizeOrig = getStackSize(stackCursor);
                dragSplitItemsIntoSlots(gui, targetSlots);
                stackCursor = player.inventory.getItemStack();

                // Items left in cursor
                if (isStackEmpty(stackCursor) == false)
                {
                    // Didn't manage to move any items anymore
                    if (getStackSize(stackCursor) >= sizeOrig)
                    {
                        break;
                    }

                    // Collect all the remaining items into the first found slot, as long as possible
                    leftClickSlot(gui, slotReturn);

                    // All of them didn't fit into the first slot anymore, switch into the current source slot
                    if (isStackEmpty(player.inventory.getItemStack()) == false)
                    {
                        slotReturn = slotNum;
                        leftClickSlot(gui, slotReturn);
                    }
                }
            }
            // Failed to pick up the stack, break to avoid infinite loops
            // TODO: we could also "blacklist" this slot and try to continue...?
            else
            {
                break;
            }

            // Somehow items were left in the cursor, break here
            if (isStackEmpty(player.inventory.getItemStack()) == false)
            {
                break;
            }
        }

        // Return the rest of the items to the original slot
        if (slotNum >= 0 && isStackEmpty(player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotNum);
        }
    }

    public static void rightClickCraftOneStack(GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        Minecraft mc = Minecraft.getMinecraft();
        InventoryPlayer inv = mc.player.inventory;
        ItemStack stackCursor = inv.getItemStack();

        if (slot == null || slot.getHasStack() == false ||
            (isStackEmpty(stackCursor) == false) && areStacksEqual(slot.getStack(), stackCursor) == false)
        {
            return;
        }

        int sizeLast = 0;

        while (true)
        {
            rightClickSlot(gui, slot.slotNumber);
            stackCursor = inv.getItemStack();

            // Failed to craft items, or the stack became full, or ran out of ingredients
            if (isStackEmpty(stackCursor) || getStackSize(stackCursor) <= sizeLast ||
                getStackSize(stackCursor) >= stackCursor.getMaxStackSize() ||
                areStacksEqual(slot.getStack(), stackCursor) == false)
            {
                break;
            }

            sizeLast = getStackSize(stackCursor);
        }
    }

    public static void craftEverythingPossibleWithCurrentRecipe(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (slot != null && isStackEmpty(recipe.getResult()) == false)
        {
            SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

            if (range != null)
            {
                // Clear all items from the grid first, to avoid unbalanced stacks
                if (clearCraftingGridOfItems(recipe, gui, range, false) == false)
                {
                    return;
                }

                tryMoveItemsToCraftingGridSlots(recipe, slot, gui, true);

                if (slot.getHasStack())
                {
                    craftAsManyItemsAsPossible(recipe, slot, gui);
                }
            }
        }
    }

    public static void moveAllCraftingResultsToOtherInventory(CraftingRecipe recipe, GuiContainer gui)
    {
        if (isStackEmpty(recipe.getResult()) == false)
        {
            Slot slot = null;
            ItemStack stackResult = recipe.getResult().copy();

            for (Slot slotTmp : gui.inventorySlots.inventorySlots)
            {
                // This slot is likely in the player inventory, as there is another inventory above
                if (areStacksEqual(slotTmp.getStack(), stackResult) &&
                    inventoryExistsAbove(slotTmp, gui.inventorySlots))
                {
                    slot = slotTmp;
                    break;
                }
            }

            if (slot != null)
            {
                // Get a list of slots with matching items, which are in the same inventory
                // as the slot that is assumed to be in the player inventory.
                List<Integer> slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, true, stackResult, false, false, false);

                for (int slotNum : slots)
                {
                    shiftClickSlot(gui, slotNum);
                }
            }
        }
    }

    public static void throwAllCraftingResultsToGround(CraftingRecipe recipe, GuiContainer gui)
    {
        Slot slot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

        if (slot != null && isStackEmpty(recipe.getResult()) == false)
        {
            dropStacks(gui, recipe.getResult(), slot, false);
        }
    }

    private static int putSingleItemIntoSlots(GuiContainer gui, List<Integer> targetSlots, int startIndex)
    {
        Minecraft mc = Minecraft.getMinecraft();
        ItemStack stackInCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackInCursor))
        {
            return 0;
        }

        int numSlots = gui.inventorySlots.inventorySlots.size();
        int numItems = getStackSize(stackInCursor);
        int loops = Math.min(numItems, targetSlots.size() - startIndex);
        int count = 0;

        for (int i = 0; i < loops; i++)
        {
            int slotNum = targetSlots.get(startIndex + i);

            if (slotNum >= numSlots)
            {
                break;
            }

            rightClickSlot(gui, slotNum);
            count++;
        }

        return count;
    }

    public static void moveOneSetOfItemsFromSlotToOtherInventory(GuiContainer gui, Slot slot)
    {
        leftClickSlot(gui, slot.slotNumber);

        Minecraft mc = Minecraft.getMinecraft();
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackCursor) == false)
        {
            List<Integer> slots = getSlotNumbersOfMatchingStacks(gui.inventorySlots, slot, false, stackCursor, true, true, false);

            if (moveItemFromCursorToSlots(gui, slots) == false)
            {
                slots = getSlotNumbersOfEmptySlots(gui.inventorySlots, slot, false, true, false);
                moveItemFromCursorToSlots(gui, slots);
            }
        }
    }

    public static void moveOneRecipeItemIntoCraftingGrid(GuiContainer gui, Slot slotGridFirst, ItemStack ingredientReference, List<Integer> targetSlots)
    {
        Container container = gui.inventorySlots;
        Minecraft mc = Minecraft.getMinecraft();
        int index = 0;
        int slotNum = -1;
        int slotCount = targetSlots.size();

        while (index < slotCount)
        {
            slotNum = getSlotNumberOfSmallestStackFromDifferentInventory(container, slotGridFirst, ingredientReference, slotCount);

            // Didn't find ingredient items
            if (slotNum < 0)
            {
                break;
            }

            // Pick up the ingredient stack from the found slot
            leftClickSlot(gui, slotNum);

            // Successfully picked up ingredient items
            if (areStacksEqual(ingredientReference, mc.player.inventory.getItemStack()))
            {
                int filled = putSingleItemIntoSlots(gui, targetSlots, index);
                index += filled;

                if (filled < 1)
                {
                    break;
                }
            }
            // Failed to pick up the stack, break to avoid infinite loops
            // TODO: we could also "blacklist" this slot and try to continue...?
            else
            {
                break;
            }
        }

        // Return the rest of the items to the original slot
        if (slotNum >= 0 && isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotNum);
        }
    }

    private static boolean moveItemFromCursorToSlots(GuiContainer gui, List<Integer> slotNumbers)
    {
        Minecraft mc = Minecraft.getMinecraft();
        InventoryPlayer inv = mc.player.inventory;

        for (int slotNum : slotNumbers)
        {
            leftClickSlot(gui, slotNum);

            if (isStackEmpty(inv.getItemStack()))
            {
                return true;
            }
        }

        return false;
    }

    private static void moveItemsFromInventory(GuiContainer gui, int slotTo, IInventory invSrc, ItemStack stackTemplate, boolean fillStacks)
    {
        Container container = gui.inventorySlots;

        for (Slot slot : container.inventorySlots)
        {
            if (slot == null)
            {
                continue;
            }

            if (slot.inventory == invSrc && areStacksEqual(stackTemplate, slot.getStack()))
            {
                if (fillStacks)
                {
                    if (clickSlotsToMoveItems(gui, slot.slotNumber, slotTo) == false)
                    {
                        break;
                    }
                }
                else
                {
                    clickSlotsToMoveSingleItem(gui, slot.slotNumber, slotTo);
                    break;
                }
            }
        }
    }

    private static int getSlotNumberOfLargestMatchingStackFromDifferentInventory(Container container, Slot slotReference, ItemStack stackReference)
    {
        int slotNum = -1;
        int largest = 0;

        for (Slot slot : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                areStacksEqual(stackReference, slot.getStack()))
            {
                int stackSize = getStackSize(slot.getStack());

                if (stackSize > largest)
                {
                    slotNum = slot.slotNumber;
                    largest = stackSize;
                }
            }
        }

        return slotNum;
    }

    /**
     * Returns the slot number of the slot that has the smallest stackSize that is still equal to or larger
     * than idealSize. The slot must also NOT be in the same inventory as slotReference.
     * If an adequately large stack is not found, then the largest one is selected.
     * @param container
     * @param slotReference
     * @param stackReference
     * @return
     */
    private static int getSlotNumberOfSmallestStackFromDifferentInventory(Container container, Slot slotReference, ItemStack stackReference, int idealSize)
    {
        int slotNum = -1;
        int smallest = Integer.MAX_VALUE;

        for (Slot slot : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                areStacksEqual(stackReference, slot.getStack()))
            {
                int stackSize = getStackSize(slot.getStack());

                if (stackSize < smallest && stackSize >= idealSize)
                {
                    slotNum = slot.slotNumber;
                    smallest = stackSize;
                }
            }
        }

        // Didn't find an adequately sized stack, now try to find at least some items...
        if (slotNum == -1)
        {
            int largest = 0;

            for (Slot slot : container.inventorySlots)
            {
                if (areSlotsInSameInventory(slot, slotReference) == false && slot.getHasStack() &&
                    areStacksEqual(stackReference, slot.getStack()))
                {
                    int stackSize = getStackSize(slot.getStack());

                    if (stackSize > largest)
                    {
                        slotNum = slot.slotNumber;
                        largest = stackSize;
                    }
                }
            }
        }

        return slotNum;
    }

    /**
     * Return the slot numbers of slots that have items identical to stackReference, that are NOT in the same
     * inventory as slotReference. If preferPartial is true, then stacks with a stackSize less that getMaxStackSize() are
     * at the beginning of the list (not ordered though) and full stacks are at the end, otherwise the reverse is true.
     * @param container
     * @param slotReference
     * @param sameInventory if true, then the returned slots are from the same inventory, if false, then from a different inventory
     * @param stackReference
     * @param preferPartial
     * @param treatHotbarAsDifferent
     * @param reverse if true, returns the slots starting from the end of the inventory
     * @return
     */
    private static List<Integer> getSlotNumbersOfMatchingStacks(
            Container container, Slot slotReference, boolean sameInventory,
            ItemStack stackReference, boolean preferPartial, boolean treatHotbarAsDifferent, boolean reverse)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;
        final int increment = reverse ? -1 : 1;

        for (int i = reverse ? maxSlot : 0; i >= 0 && i <= maxSlot; i += increment)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && slot.getHasStack() &&
                areSlotsInSameInventory(slot, slotReference, treatHotbarAsDifferent) == sameInventory &&
                areStacksEqual(slot.getStack(), stackReference))
            {
                if ((getStackSize(slot.getStack()) < stackReference.getMaxStackSize()) == preferPartial)
                {
                    slots.add(0, slot.slotNumber);
                }
                else
                {
                    slots.add(slot.slotNumber);
                }
            }
        }

        return slots;
    }

    private static List<Integer> getSlotNumbersOfEmptySlots(
            Container container, Slot slotReference, boolean sameInventory, boolean treatHotbarAsDifferent, boolean reverse)
    {
        List<Integer> slots = new ArrayList<Integer>(64);
        final int maxSlot = container.inventorySlots.size() - 1;
        final int increment = reverse ? -1 : 1;

        for (int i = reverse ? maxSlot : 0; i >= 0 && i <= maxSlot; i += increment)
        {
            Slot slot = container.getSlot(i);

            if (slot != null && slot.getHasStack() == false &&
                areSlotsInSameInventory(slot, slotReference, treatHotbarAsDifferent) == sameInventory)
            {
                slots.add(slot.slotNumber);
            }
        }

        return slots;
    }

    public static boolean areStacksEqual(ItemStack stack1, ItemStack stack2)
    {
        return ItemStack.areItemsEqual(stack1, stack2) && ItemStack.areItemStackTagsEqual(stack1, stack2);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2)
    {
        return areSlotsInSameInventory(slot1, slot2, false);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2, boolean treatHotbarAsDifferent)
    {
        if (slot1.inventory == slot2.inventory)
        {
            if (treatHotbarAsDifferent && slot1.inventory instanceof InventoryPlayer)
            {
                int index1 = AccessorUtils.getSlotIndex(slot1);
                int index2 = AccessorUtils.getSlotIndex(slot2);
                // Don't ever treat the offhand slot as a different inventory
                return index1 == 40 || index2 == 40 || (index1 < 9) == (index2 < 9);
            }

            return true;
        }

        return false;
    }

    private static ItemStack[] getOriginalStacks(Container container)
    {
        ItemStack[] originalStacks = new ItemStack[container.inventorySlots.size()];

        for (int i = 0; i < originalStacks.length; i++)
        {
            originalStacks[i] = container.inventorySlots.get(i).getStack().copy();
        }

        return originalStacks;
    }

    private static void restoreOriginalStacks(Container container, ItemStack[] originalStacks)
    {
        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackSlot = container.getSlot(i).getStack();

            if (areStacksEqual(stackSlot, originalStacks[i]) == false ||
                (isStackEmpty(stackSlot) == false && getStackSize(stackSlot) != getStackSize(originalStacks[i])))
            {
                container.putStackInSlot(i, originalStacks[i]);
            }
        }
    }

    private static int getTargetSlot(Container container, ItemStack[] originalStacks)
    {
        List<Slot> slots = container.inventorySlots;

        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackOrig = originalStacks[i];
            ItemStack stackNew = slots.get(i).getStack();

            if ((isStackEmpty(stackOrig) && isStackEmpty(stackNew) == false) ||
               (isStackEmpty(stackOrig) == false && isStackEmpty(stackNew) == false &&
               getStackSize(stackNew) == (getStackSize(stackOrig) + 1)))
            {
                return i;
            }
        }

        return -1;
    }

    /*
    private void clickSlotsToMoveItems(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory)
    {
        for (Slot slotTmp : gui.inventorySlots.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber && areSlotsInSameInventory(slotTmp, slot) == toOtherInventory &&
                slotTmp.getHasStack() && (matchingOnly == false || areStacksEqual(slot.getStack(), slotTmp.getStack())))
            {
                this.clickSlotsToMoveItemsFromSlot(slotTmp, gui, toOtherInventory);
                return;
            }
        }

        // Move the hovered-over slot's stack last
        if (toOtherInventory)
        {
            this.clickSlotsToMoveItemsFromSlot(slot, gui, toOtherInventory);
        }
    }
    */

    private static void clickSlotsToMoveItemsFromSlot(Slot slotFrom, GuiContainer gui, boolean toOtherInventory)
    {
        Minecraft mc = Minecraft.getMinecraft();
        EntityPlayer player = mc.player;
        // Left click to pick up the found source stack
        leftClickSlot(gui, slotFrom.slotNumber);

        if (isStackEmpty(player.inventory.getItemStack()))
        {
            return;
        }

        for (Slot slotDst : gui.inventorySlots.inventorySlots)
        {
            ItemStack stackDst = slotDst.getStack();

            if (areSlotsInSameInventory(slotDst, slotFrom) != toOtherInventory &&
                (isStackEmpty(stackDst) || areStacksEqual(stackDst, player.inventory.getItemStack())))
            {
                // Left click to (try and) place items to the slot
                leftClickSlot(gui, slotDst.slotNumber);
            }

            if (isStackEmpty(player.inventory.getItemStack()))
            {
                return;
            }
        }

        // Couldn't fit the entire stack to the target inventory, return the rest of the items
        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }
    }

    private static boolean clickSlotsToMoveSingleItem(GuiContainer gui, int slotFrom, int slotTo)
    {
        //System.out.println("clickSlotsToMoveSingleItem(from: " + slotFrom + ", to: " + slotTo + ")");
        Minecraft mc = Minecraft.getMinecraft();
        ItemStack stack = gui.inventorySlots.inventorySlots.get(slotFrom).getStack();

        if (isStackEmpty(stack))
        {
            return false;
        }

        // Click on the from-slot to take items to the cursor - if there is more than one item in the from-slot,
        // right click on it, otherwise left click.
        if (getStackSize(stack) > 1)
        {
            rightClickSlot(gui, slotFrom);
        }
        else
        {
            leftClickSlot(gui, slotFrom);
        }

        // Right click on the target slot to put one item to it
        rightClickSlot(gui, slotTo);

        // If there are items left in the cursor, then return them back to the original slot
        if (isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // Left click again on the from-slot to return the rest of the items to it
            leftClickSlot(gui, slotFrom);
        }

        return true;
    }

    private static boolean clickSlotsToMoveSingleItemByShiftClick(GuiContainer gui, int slotFrom)
    {
        Minecraft mc = Minecraft.getMinecraft();
        Slot slot = gui.inventorySlots.inventorySlots.get(slotFrom);
        ItemStack stack = slot.getStack();

        if (isStackEmpty(stack))
        {
            return false;
        }

        if (getStackSize(stack) > 1)
        {
            // Left click on the from-slot to take all the items to the cursor
            leftClickSlot(gui, slotFrom);

            // Still items left in the slot, put the stack back and abort
            if (slot.getHasStack())
            {
                leftClickSlot(gui, slotFrom);
                return false;
            }
            else
            {
                // Right click one item back to the slot
                rightClickSlot(gui, slotFrom);
            }
        }

        // ... and then shift-click on the slot
        shiftClickSlot(gui, slotFrom);

        if (isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // ... and then return the rest of the items
            leftClickSlot(gui, slotFrom);
        }

        return true;
    }

    /**
     * Try move items from slotFrom to slotTo
     * @return true if at least some items were moved
     */
    private static boolean clickSlotsToMoveItems(GuiContainer gui, int slotFrom, int slotTo)
    {
        Minecraft mc = Minecraft.getMinecraft();
        EntityPlayer player = mc.player;
        //System.out.println("clickSlotsToMoveItems(from: " + slotFrom + ", to: " + slotTo + ")");

        // Left click to take items
        leftClickSlot(gui, slotFrom);

        // Couldn't take the items, bail out now
        if (isStackEmpty(player.inventory.getItemStack()))
        {
            return false;
        }

        boolean ret = true;
        int size = getStackSize(player.inventory.getItemStack());

        // Left click on the target slot to put the items to it
        leftClickSlot(gui, slotTo);

        // If there are items left in the cursor, then return them back to the original slot
        if (isStackEmpty(player.inventory.getItemStack()) == false)
        {
            ret = getStackSize(player.inventory.getItemStack()) != size;

            // Left click again on the from-slot to return the rest of the items to it
            leftClickSlot(gui, slotFrom);
        }

        return ret;
    }

    private static boolean shiftClickSlotWithCheck(GuiContainer gui, int slotNum)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);

        if (slot == null || slot.getHasStack() == false)
        {
            return false;
        }

        int sizeOrig = getStackSize(slot.getStack());
        shiftClickSlot(gui, slotNum);

        return slot.getHasStack() == false || getStackSize(slot.getStack()) != sizeOrig;
    }

    public static boolean tryMoveItemsVertically(GuiContainer gui, Slot slot, RecipeStorage recipes, boolean moveUp, MoveAmount amount)
    {
        Minecraft mc = Minecraft.getMinecraft();

        // We require an empty cursor
        if (slot == null || isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            return false;
        }

        // Villager handling only happens when scrolling over the trade output slot
        boolean villagerHandling = Toggles.SCROLL_VILLAGER.getValue() && gui instanceof GuiMerchant && slot instanceof SlotMerchantResult;
        boolean craftingHandling = Toggles.SCROLL_CRAFT.getValue() && isCraftingSlot(gui, slot);
        boolean isCtrlDown = GuiContainer.isCtrlKeyDown();

        if (craftingHandling)
        {
            return tryMoveItemsCrafting(recipes, slot, gui, moveUp == false, amount == MoveAmount.MOVE_ALL, isCtrlDown);
        }

        if (villagerHandling)
        {
            return tryMoveItemsVillager((GuiMerchant) gui, slot, moveUp == false, amount == MoveAmount.MOVE_ALL);
        }

        List<Integer> slots = getVerticallyFurthestSuitableSlotsForStackInSlot(gui.inventorySlots, slot, moveUp);

        if (slots.isEmpty())
        {
            return false;
        }

        if (amount == MoveAmount.MOVE_ALL)
        {
            moveStackToSlots(gui, slot, slots, false);
        }
        else if (amount == MoveAmount.MOVE_ONE)
        {
            moveOneItemToFirstValidSlot(gui, slot, slots);
        }
        else if (amount == MoveAmount.LEAVE_ONE)
        {
            moveStackToSlots(gui, slot, slots, true);
        }

        return true;
    }

    private static void moveStackToSlots(GuiContainer gui, Slot slotFrom, List<Integer> slotsTo, boolean leaveOne)
    {
        Minecraft mc = Minecraft.getMinecraft();
        InventoryPlayer inv = mc.player.inventory;

        // Pick up the stack
        leftClickSlot(gui, slotFrom.slotNumber);

        if (leaveOne)
        {
            rightClickSlot(gui, slotFrom.slotNumber);
        }

        for (int slotNum : slotsTo)
        {
            if (isStackEmpty(inv.getItemStack()))
            {
                break;
            }

            leftClickSlot(gui, slotNum);
            //System.out.printf("suitable slot: %3d\n", slotNum);
        }

        // Return the rest of the items, if any
        if (isStackEmpty(inv.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }
    }

    private static void moveOneItemToFirstValidSlot(GuiContainer gui, Slot slotFrom, List<Integer> slotsTo)
    {
        Minecraft mc = Minecraft.getMinecraft();
        InventoryPlayer inv = mc.player.inventory;

        // Pick up half of the the stack
        rightClickSlot(gui, slotFrom.slotNumber);

        if (isStackEmpty(inv.getItemStack()))
        {
            return;
        }

        int sizeOrig = getStackSize(inv.getItemStack());

        for (int slotNum : slotsTo)
        {
            rightClickSlot(gui, slotNum);
            ItemStack stackCursor = inv.getItemStack();

            if (isStackEmpty(stackCursor) || getStackSize(stackCursor) != sizeOrig)
            {
                break;
            }
        }

        // Return the rest of the items, if any
        if (isStackEmpty(inv.getItemStack()) == false)
        {
            leftClickSlot(gui, slotFrom.slotNumber);
        }
    }

    private static List<Integer> getVerticallyFurthestSuitableSlotsForStackInSlot(Container container, Slot slot, boolean above)
    {
        if (slot == null || slot.getHasStack() == false)
        {
            return Collections.emptyList();
        }

        List<SlotVerticalSorter> slotSorters = new ArrayList<SlotVerticalSorter>();
        ItemStack stackSlot = slot.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber && slotTmp.yPos != slot.yPos)
            {
                if (above == slotTmp.yPos < slot.yPos)
                {
                    ItemStack stackTmp = slotTmp.getStack();

                    if ((isStackEmpty(stackTmp) && slotTmp.isItemValid(stackSlot)) ||
                        (areStacksEqual(stackTmp, stackSlot)) && slotTmp.getItemStackLimit(stackTmp) > getStackSize(stackTmp))
                    {
                        slotSorters.add(new SlotVerticalSorter(slotTmp));
                    }
                }
            }
        }

        Collections.sort(slotSorters);

        if (above == false)
        {
            Collections.reverse(slotSorters);
        }

        List<Integer> slots = new ArrayList<Integer>();

        for (SlotVerticalSorter entry : slotSorters)
        {
            slots.add(entry.getSlot().slotNumber);
        }

        return slots;
    }

    private static class SlotVerticalSorter implements Comparable<SlotVerticalSorter>
    {
        private final Slot slot;

        public SlotVerticalSorter(Slot slot)
        {
            this.slot = slot;
        }

        public Slot getSlot()
        {
            return this.slot;
        }

        @Override
        public int compareTo(SlotVerticalSorter other)
        {
            if (this.getSlot().yPos == other.getSlot().yPos)
            {
                return this.getSlot().slotNumber < other.getSlot().slotNumber ? -1 : 1;
            }

            return (this.getSlot().yPos < other.getSlot().yPos) ? -1 : 1;
        }
    }

    public static void clickSlot(GuiContainer gui, int slotNum, int mouseButton, ClickType type)
    {
        if (slotNum >= 0 && slotNum < gui.inventorySlots.inventorySlots.size())
        {
            Slot slot = gui.inventorySlots.getSlot(slotNum);
            clickSlot(gui, slot, slotNum, mouseButton, type);
        }
        else
        {
            try
            {
                Minecraft mc = Minecraft.getMinecraft();
                mc.playerController.windowClick(gui.inventorySlots.windowId, slotNum, mouseButton, type, mc.player);
            }
            catch (Exception e)
            {
                LiteModItemScroller.logger.warn("Exception while emulating a slot click: gui: '{}', slotNum: {}, mouseButton; {}, ClickType: {}",
                        gui.getClass().getName(), slotNum, mouseButton, type, e);
            }
        }
    }

    public static void clickSlot(GuiContainer gui, Slot slot, int slotNum, int mouseButton, ClickType type)
    {
        try
        {
            AccessorUtils.handleMouseClick(gui, slot, slotNum, mouseButton, type);
        }
        catch (Exception e)
        {
            LiteModItemScroller.logger.warn("Exception while emulating a slot click: gui: '{}', slotNum: {}, mouseButton; {}, ClickType: {}",
                    gui.getClass().getName(), slotNum, mouseButton, type, e);
        }
    }

    public static void leftClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 0, ClickType.PICKUP);
    }

    public static void rightClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 1, ClickType.PICKUP);
    }

    public static void shiftClickSlot(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 0, ClickType.QUICK_MOVE);
    }

    public static void dropItemsFromCursor(GuiContainer gui)
    {
        clickSlot(gui, -999, 0, ClickType.PICKUP);
    }

    public static void dropStack(GuiContainer gui, int slotNum)
    {
        clickSlot(gui, slotNum, 1, ClickType.THROW);
    }

    private static void dragSplitItemsIntoSlots(GuiContainer gui, List<Integer> targetSlots)
    {
        Minecraft mc = Minecraft.getMinecraft();
        ItemStack stackInCursor = mc.player.inventory.getItemStack();

        if (isStackEmpty(stackInCursor))
        {
            return;
        }

        if (targetSlots.size() == 1)
        {
            leftClickSlot(gui, targetSlots.get(0));
            return;
        }

        int numSlots = gui.inventorySlots.inventorySlots.size();
        int loops = targetSlots.size();

        // Start the drag
        clickSlot(gui, -999, 0, ClickType.QUICK_CRAFT);

        for (int i = 0; i < loops; i++)
        {
            int slotNum = targetSlots.get(i);

            if (slotNum >= numSlots)
            {
                break;
            }

            clickSlot(gui, targetSlots.get(i), 1, ClickType.QUICK_CRAFT);
        }

        // End the drag
        clickSlot(gui, -999, 2, ClickType.QUICK_CRAFT);
    }

    /**************************************************************
     * Compatibility code for pre-1.11 vs. 1.11+
     * Well kind of, as in make the differences minimal,
     * only requires changing these things for the ItemStack
     * related changes.
     *************************************************************/

    public static final ItemStack EMPTY_STACK = ItemStack.EMPTY;

    public static boolean isStackEmpty(ItemStack stack)
    {
        return stack.isEmpty();
    }

    public static int getStackSize(ItemStack stack)
    {
        return stack.getCount();
    }

    public static void setStackSize(ItemStack stack, int size)
    {
        stack.setCount(size);
    }
}
