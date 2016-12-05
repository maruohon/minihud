package fi.dy.masa.itemscroller.event;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.lwjgl.input.Mouse;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotMerchantResult;
import net.minecraft.item.ItemStack;
import net.minecraft.village.MerchantRecipe;
import net.minecraft.village.MerchantRecipeList;
import net.minecraftforge.client.event.GuiScreenEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.ReflectionHelper.UnableToAccessFieldException;
import net.minecraftforge.fml.relauncher.ReflectionHelper.UnableToFindMethodException;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import net.minecraftforge.items.SlotItemHandler;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.config.Configs;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private int lastPosX;
    private int lastPosY;
    private int slotNumberLast;
    private final Set<Integer> draggedSlots = new HashSet<Integer>();
    private final Field fieldGuiLeft;
    private final Field fieldGuiTop;
    private final Field fieldGuiXSize;
    private final Field fieldGuiYSize;

    public InputEventHandler()
    {
        this.fieldGuiLeft =  ReflectionHelper.findField(GuiContainer.class, "field_147003_i", "guiLeft");
        this.fieldGuiTop =   ReflectionHelper.findField(GuiContainer.class, "field_147009_r", "guiTop");
        this.fieldGuiXSize = ReflectionHelper.findField(GuiContainer.class, "field_146999_f", "xSize");
        this.fieldGuiYSize = ReflectionHelper.findField(GuiContainer.class, "field_147000_g", "ySize");
    }

    @SubscribeEvent
    public void onMouseInputEvent(GuiScreenEvent.MouseInputEvent.Pre event)
    {
        int dWheel = Mouse.getEventDWheel();
        GuiScreen gui = event.getGui();

        if (gui instanceof GuiContainerCreative)
        {
            return;
        }

        if (gui instanceof GuiContainer)
        {
            boolean cancel = false;

            if (dWheel != 0)
            {
                this.tryMoveItems((GuiContainer) gui, dWheel > 0);
            }
            else
            {
                if (Configs.enableShiftPlaceItems && this.canShiftPlaceItems((GuiContainer) gui))
                {
                    cancel = this.shiftPlaceItems((GuiContainer) gui);
                }
                else if (Configs.enableShiftDropItems && this.canShiftDropItems((GuiContainer) gui))
                {
                    this.shiftDropItems((GuiContainer) gui);
                }
                else if (Configs.enableDragMovingShiftLeft || Configs.enableDragMovingShiftRight || Configs.enableDragMovingControlLeft)
                {
                    cancel = this.dragMoveItems((GuiContainer) gui);
                }
            }

            if (cancel && event.isCancelable())
            {
                event.setCanceled(true);
            }
        }
    }

    private boolean isValidSlot(Slot slot, GuiContainer gui, boolean requireItems)
    {
        if (gui.inventorySlots == null || gui.inventorySlots.inventorySlots == null)
        {
            return false;
        }

        return slot != null && gui.inventorySlots.inventorySlots.contains(slot) && (requireItems == false || slot.getHasStack());
    }

    /**
     * Checks if there are slots belonging to another inventory on screen above the given slot
     */
    private boolean inventoryExistsAbove(Slot slot, Container container)
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

    private boolean canShiftPlaceItems(GuiContainer gui)
    {
        boolean eventKeyIsLeftButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();

        if (GuiScreen.isShiftKeyDown() == false || eventKeyIsLeftButton == false)
        {
            return false;
        }

        Slot slot = gui.getSlotUnderMouse();
        ItemStack stackCursor = gui.mc.player.inventory.getItemStack();

        // The target slot needs to be an empty, valid slot, and there needs to be items in the cursor
        return slot != null && stackCursor.isEmpty() == false && this.isValidSlot(slot, gui, false) &&
               slot.getHasStack() == false && slot.isItemValid(stackCursor);
    }

    private boolean shiftPlaceItems(GuiContainer gui)
    {
        Slot slot = gui.getSlotUnderMouse();

        // Left click to place the items from the cursor to the slot
        gui.mc.playerController.windowClick(gui.inventorySlots.windowId, slot.slotNumber, 0, ClickType.PICKUP, gui.mc.player);

        this.tryMoveStacks(slot, gui, true, false, false);

        return true;
    }

    private boolean canShiftDropItems(GuiContainer gui)
    {
        boolean eventKeyIsLeftButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();

        if (GuiScreen.isShiftKeyDown() == false || eventKeyIsLeftButton == false)
        {
            return false;
        }

        int left = 0;
        int top = 0;
        int xSize = 0;
        int ySize = 0;
        int mouseAbsX = 0;
        int mouseAbsY = 0;

        try
        {
            left = this.fieldGuiLeft.getInt(gui);
            top = this.fieldGuiTop.getInt(gui);
            xSize = this.fieldGuiXSize.getInt(gui);
            ySize = this.fieldGuiYSize.getInt(gui);
            mouseAbsX = Mouse.getEventX() * gui.width / gui.mc.displayWidth;
            mouseAbsY = gui.height - Mouse.getEventY() * gui.height / gui.mc.displayHeight - 1;
        }
        catch (IllegalAccessException e)
        {
            ItemScroller.logger.warn("Failed to reflect GuiContainer#guiLeft or guiTop or xSize or ySize");
        }

        boolean isOutsideGui = mouseAbsX < left || mouseAbsY < top || mouseAbsX >= left + xSize || mouseAbsY >= top + ySize;

        return isOutsideGui && this.getSlotAtPosition(gui, mouseAbsX - left, mouseAbsY - top) == null &&
                gui.mc.player.inventory.getItemStack().isEmpty() == false;
    }

    private void shiftDropItems(GuiContainer gui)
    {
        EntityPlayer player = gui.mc.player;
        ItemStack stackCursor = player.inventory.getItemStack();
        Container container = gui.inventorySlots;

        // First drop the existing stack from the cursor
        gui.mc.playerController.windowClick(container.windowId, -999, 0, ClickType.PICKUP, player);

        for (Slot slot : container.inventorySlots)
        {
            if (areStacksEqual(slot.getStack(), stackCursor))
            {
                // Pick up the items
                gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 0, ClickType.PICKUP, player);
                // Drop the items
                gui.mc.playerController.windowClick(container.windowId, -999, 0, ClickType.PICKUP, player);
            }
        }
    }

    private boolean dragMoveItems(GuiContainer gui)
    {
        if (gui.mc.player.inventory.getItemStack().isEmpty() == false)
        {
            return false;
        }

        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();
        boolean eitherMouseButtonDown = leftButtonDown || rightButtonDown;
        boolean eventKeyIsLeftButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();
        boolean eventKeyIsRightButton = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindUseItem.getKeyCode();
        boolean eventButtonState = Mouse.getEventButtonState();

        if ((isShiftDown && leftButtonDown && Configs.enableDragMovingShiftLeft == false) ||
            (isShiftDown && rightButtonDown && Configs.enableDragMovingShiftRight == false) ||
            (isControlDown && eitherMouseButtonDown && Configs.enableDragMovingControlLeft == false))
        {
            return false;
        }

        boolean leaveOneItem = leftButtonDown == false;
        boolean moveOnlyOne = isShiftDown == false;
        int mouseX = Mouse.getEventX() * gui.width / gui.mc.displayWidth;
        int mouseY = gui.height - Mouse.getEventY() * gui.height / gui.mc.displayHeight - 1;
        boolean cancel = false;

        if (eventButtonState)
        {
            if (((eventKeyIsLeftButton || eventKeyIsRightButton) && isControlDown && Configs.enableDragMovingControlLeft) ||
                (eventKeyIsRightButton && isShiftDown && Configs.enableDragMovingShiftRight))
            {
                // Reset this or the method call won't do anything...
                this.slotNumberLast = -1;
                cancel = this.dragMoveFromSlotAtPosition(gui, mouseX, mouseY, leaveOneItem, moveOnlyOne);
            }
        }

        // Check that either mouse button is down
        if (cancel == false && (isShiftDown || isControlDown) && eitherMouseButtonDown)
        {
            int distX = mouseX - this.lastPosX;
            int distY = mouseY - this.lastPosY;
            int absX = Math.abs(distX);
            int absY = Math.abs(distY);

            if (absX > absY)
            {
                int inc = distX > 0 ? 1 : -1;

                for (int x = this.lastPosX; ; x += inc)
                {
                    int y = absX != 0 ? this.lastPosY + ((x - this.lastPosX) * distY / absX) : mouseY;
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

                    if (x == mouseX)
                    {
                        break;
                    }
                }
            }
            else
            {
                int inc = distY > 0 ? 1 : -1;

                for (int y = this.lastPosY; ; y += inc)
                {
                    int x = absY != 0 ? this.lastPosX + ((y - this.lastPosY) * distX / absY) : mouseX;
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

                    if (y == mouseY)
                    {
                        break;
                    }
                }
            }
        }

        this.lastPosX = mouseX;
        this.lastPosY = mouseY;

        // Always update the slot under the mouse.
        // This should prevent a "double click/move" when shift + left clicking on slots that have more
        // than one stack of items. (the regular slotClick() + a "drag move" from the slot that is under the mouse
        // when the left mouse button is pressed down and this code runs).
        Slot slot = this.getSlotAtPosition(gui, mouseX, mouseY);
        this.slotNumberLast = slot != null ? slot.slotNumber : -1;

        if (eitherMouseButtonDown == false)
        {
            this.draggedSlots.clear();
        }

        return cancel;
    }

    private boolean dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y, boolean leaveOneItem, boolean moveOnlyOne)
    {
        Slot slot = this.getSlotAtPosition(gui, x, y);
        boolean success = false;

        if (slot != null && slot.slotNumber != this.slotNumberLast && this.isValidSlot(slot, gui, true))
        {
            if (this.draggedSlots.contains(slot.slotNumber) == false)
            {
                if (moveOnlyOne)
                {
                    success = this.tryMoveSingleItemToOtherInventory(slot, gui);
                }
                else if (leaveOneItem)
                {
                    success = this.tryMoveAllButOneItemToOtherInventory(slot, gui);
                }
                else
                {
                    this.shiftClickSlot(gui.inventorySlots, gui.mc, slot.slotNumber);
                    success = true;
                }

                this.draggedSlots.add(slot.slotNumber);
            }

            // Cancel the event to prevent further processing (and thus a transferStackInSlot() call)
            return success;
        }

        return success;
    }

    private Slot getSlotAtPosition(GuiContainer gui, int x, int y)
    {
        try
        {
            Method method = ReflectionHelper.findMethod(GuiContainer.class, gui,
                new String[] { "func_146975_c", "getSlotAtPosition" }, int.class, int.class);

            return (Slot) method.invoke(gui, x, y);
        }
        catch (UnableToFindMethodException e)
        {
            ItemScroller.logger.error("Error while trying reflect GuiContainer#getSlotAtPosition() from {} (UnableToFindMethodException)", gui.getClass().getSimpleName());
            e.printStackTrace();
        }
        catch (InvocationTargetException e)
        {
            ItemScroller.logger.error("Error while trying reflect GuiContainer#getSlotAtPosition() from {} (InvocationTargetException)", gui.getClass().getSimpleName());
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            ItemScroller.logger.error("Error while trying reflect GuiContainer#getSlotAtPosition() from {} (IllegalAccessException)", gui.getClass().getSimpleName());
            e.printStackTrace();
        }

        return null;
    }

    private boolean tryMoveItemsVillager(GuiMerchant gui, Slot slot, boolean moveToOtherInventory, boolean isShiftDown)
    {
        if (isShiftDown == true)
        {
            // Try to fill the merchant's buy slots from the player inventory
            if (moveToOtherInventory == false)
            {
                this.tryMoveItemsToMerchantBuySlots(gui, true);
            }
            // Move items from sell slot to player inventory
            else if (slot.getHasStack())
            {
                this.tryMoveStacks(slot, gui, true, true, true);
            }
            // Scrolling over an empty output slot, clear the buy slots
            else
            {
                this.tryMoveStacks(slot, gui, false, true, false);
            }
        }
        else
        {
            // Scrolling items from player inventory into merchant buy slots
            if (moveToOtherInventory == false)
            {
                this.tryMoveItemsToMerchantBuySlots(gui, false);
            }
            // Scrolling items from this slot/inventory into the other inventory
            else if (slot.getHasStack())
            {
                this.tryMoveSingleItemToOtherInventory(slot, gui);
            }
        }

        return false;
    }

    private boolean tryMoveItems(GuiContainer gui, boolean scrollingUp)
    {
        Slot slot = gui.getSlotUnderMouse();
        boolean isCtrlDown = GuiContainer.isCtrlKeyDown();
        boolean isShiftDown = GuiContainer.isShiftKeyDown();
        // Villager handling only happens when scrolling over the trade output slot
        boolean villagerHandling = Configs.enableScrollingVillager && gui instanceof GuiMerchant && slot instanceof SlotMerchantResult;

        // Check that the cursor is empty, and the slot is valid (don't require items in case of the villager output slot)
        if (gui.mc.player.inventory.getItemStack().isEmpty() == false || this.isValidSlot(slot, gui, villagerHandling ? false : true) == false)
        {
            return false;
        }

        boolean moveToOtherInventory = scrollingUp;

        if (Configs.useSlotPositionAwareScrollDirection)
        {
            boolean above = this.inventoryExistsAbove(slot, gui.inventorySlots);
            moveToOtherInventory = above == scrollingUp; // so basically: (above && scrollingUp) || (above == false && scrollingUp == false)
        }

        if ((Configs.reverseScrollDirectionSingle && isShiftDown == false) ||
            (Configs.reverseScrollDirectionStacks && isShiftDown))
        {
            moveToOtherInventory = ! moveToOtherInventory;
        }

        if (villagerHandling)
        {
            return this.tryMoveItemsVillager((GuiMerchant) gui, slot, moveToOtherInventory, isShiftDown);
        }

        if ((Configs.enableScrollingSingle == false && isShiftDown == false && isCtrlDown == false) ||
            (Configs.enableScrollingStacks == false && isShiftDown && isCtrlDown == false) ||
            (Configs.enableScrollingMatchingStacks == false && isShiftDown == false && isCtrlDown) ||
            (Configs.enableMovingEverything == false && isShiftDown && isCtrlDown))
        {
            return false;
        }

        if (isShiftDown)
        {
            // Ctrl + Shift + scroll: move everything
            if (isCtrlDown)
            {
                this.tryMoveStacks(slot, gui, false, moveToOtherInventory, false);
            }
            // Shift + scroll: move one matching stack
            else
            {
                this.tryMoveStacks(slot, gui, true, moveToOtherInventory, true);
            }
        }
        // Ctrl + scroll: Move all matching stacks
        else if (isCtrlDown)
        {
            this.tryMoveStacks(slot, gui, true, moveToOtherInventory, false);
        }
        // No Ctrl or Shift
        else
        {
            ItemStack stack = slot.getStack();

            // Scrolling items from this slot/inventory into the other inventory
            if (moveToOtherInventory)
            {
                this.tryMoveSingleItemToOtherInventory(slot, gui);
            }
            // Scrolling items from the other inventory into this slot/inventory
            else if (stack.getCount() < slot.getItemStackLimit(stack))
            {
                this.tryMoveSingleItemToThisInventory(slot, gui);
            }
        }

        return false;
    }

    private boolean tryMoveSingleItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        ItemStack stackOrig = slot.getStack();
        Container container = gui.inventorySlots;

        if (gui.mc.player.inventory.getItemStack().isEmpty() == false || slot.canTakeStack(gui.mc.player) == false ||
            (stackOrig.getCount() > 1 && slot.isItemValid(stackOrig) == false))
        {
            return false;
        }

        // Can take all the items to the cursor at once, use a shift-click method to move one item from the slot
        if (stackOrig.getCount() <= stackOrig.getMaxStackSize())
        {
            return this.clickSlotsToMoveSingleItemByShiftClick(container, gui.mc, slot.slotNumber);
        }

        ItemStack stack = stackOrig.copy();
        stack.setCount(1);

        ItemStack[] originalStacks = this.getOriginalStacks(container);

        // Try to move the temporary single-item stack via the shift-click handler method
        slot.putStack(stack);
        container.transferStackInSlot(gui.mc.player, slot.slotNumber);

        // Successfully moved the item somewhere, now we want to check where it went
        if (slot.getHasStack() == false)
        {
            int targetSlot = this.getTargetSlot(container, originalStacks);

            // Found where the item went
            if (targetSlot >= 0)
            {
                // Remove the dummy item from the target slot (on the client side)
                container.inventorySlots.get(targetSlot).decrStackSize(1);

                // Restore the original stack to the slot under the cursor (on the client side)
                this.restoreOriginalStacks(container, originalStacks);

                // Do the slot clicks to actually move the items (on the server side)
                return this.clickSlotsToMoveSingleItem(container, gui.mc, slot.slotNumber, targetSlot);
            }
        }

        // Restore the original stack to the slot under the cursor (on the client side)
        slot.putStack(stackOrig);

        return false;
    }

    private boolean tryMoveAllButOneItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        EntityPlayer player = gui.mc.player;
        ItemStack stackOrig = slot.getStack().copy();

        if (stackOrig.getCount() == 1 || stackOrig.getCount() > stackOrig.getMaxStackSize() ||
            slot.canTakeStack(player) == false || slot.isItemValid(stackOrig) == false)
        {
            return false;
        }

        Container container = gui.inventorySlots;

        // Take half of the items from the original slot to the cursor
        gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 1, ClickType.PICKUP, player);

        ItemStack stackInCursor = player.inventory.getItemStack();
        if (stackInCursor.isEmpty())
        {
            return false;
        }

        int stackInCursorSizeOrig = stackInCursor.getCount();
        int tempSlotNum = -1;

        // Find some other slot where to store one of the items temporarily
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber && slotTmp.isItemValid(stackInCursor))
            {
                ItemStack stackInSlot = slotTmp.getStack();

                if (stackInSlot.isEmpty() || areStacksEqual(stackInSlot, stackInCursor))
                {
                    // Try to put one item into the temporary slot
                    gui.mc.playerController.windowClick(container.windowId, slotTmp.slotNumber, 1, ClickType.PICKUP, player);

                    stackInCursor = player.inventory.getItemStack();

                    // Successfully stored one item
                    if (stackInCursor.isEmpty() || stackInCursor.getCount() < stackInCursorSizeOrig)
                    {
                        tempSlotNum = slotTmp.slotNumber;
                        break;
                    }
                }
            }
        }

        // Return the rest of the items into the original slot
        gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 0, ClickType.PICKUP, player);

        // Successfully stored one item in a temporary slot
        if (tempSlotNum != -1)
        {
            // Shift click the stack from the original slot
            this.shiftClickSlot(container, gui.mc, slot.slotNumber);

            // Take half a stack from the temporary slot
            gui.mc.playerController.windowClick(container.windowId, tempSlotNum, 1, ClickType.PICKUP, player);

            // Return one item into the original slot
            gui.mc.playerController.windowClick(container.windowId, slot.slotNumber, 1, ClickType.PICKUP, player);

            // Return the rest of the items to the temporary slot, if any
            if (player.inventory.getItemStack().isEmpty() == false)
            {
                gui.mc.playerController.windowClick(container.windowId, tempSlotNum, 0, ClickType.PICKUP, player);
            }

            return true;
        }

        return false;
    }

    private void tryMoveSingleItemToThisInventory(Slot slot, GuiContainer gui)
    {
        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();

        if (slot.isItemValid(stackOrig) == false)
        {
            return;
        }

        for (int slotNum = container.inventorySlots.size() - 1; slotNum >= 0; slotNum--)
        {
            Slot slotTmp = container.inventorySlots.get(slotNum);

            if (areSlotsInSameInventory(slotTmp, slot) == false &&
                slotTmp.getHasStack() && slotTmp.canTakeStack(gui.mc.player) &&
                (slotTmp.getStack().getCount() == 1 || slotTmp.isItemValid(slotTmp.getStack())))
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }

        // If we weren't able to move any items from another inventory, then try to move items
        // within the same inventory (mostly between the hotbar and the player inventory)
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                slotTmp.getHasStack() && slotTmp.canTakeStack(gui.mc.player) &&
                (slotTmp.getStack().getCount() == 1 || slotTmp.isItemValid(slotTmp.getStack())))
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig))
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }
    }

    private void tryMoveStacks(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        Container container = gui.inventorySlots;
        ItemStack stack = slot.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot) == toOtherInventory &&
                (matchingOnly == false || areStacksEqual(stack, slotTmp.getStack())))
            {
                this.shiftClickSlot(container, gui.mc, slotTmp.slotNumber);

                if (firstOnly)
                {
                    return;
                }
            }
        }

        // If moving to the other inventory, then move the hovered slot's stack last
        if (toOtherInventory)
        {
            this.shiftClickSlot(container, gui.mc, slot.slotNumber);
        }
    }

    private void tryMoveItemsToMerchantBuySlots(GuiMerchant gui, boolean fillStacks)
    {
        MerchantRecipeList list = gui.getMerchant().getRecipes(gui.mc.player);
        int index = 0;

        try
        {
            index = ReflectionHelper.getPrivateValue(GuiMerchant.class, gui, "field_147041_z", "selectedMerchantRecipe");
        }
        catch (UnableToAccessFieldException e)
        {
            ItemScroller.logger.warn("Failed to get the value of GuiMerchant.selectedMerchantRecipe");
        }

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

        if (buy1.isEmpty() == false)
        {
            this.fillBuySlot(gui, 0, buy1, fillStacks);
        }

        if (buy2.isEmpty() == false)
        {
            this.fillBuySlot(gui, 1, buy2, fillStacks);
        }
    }

    private void fillBuySlot(GuiContainer gui, int slotNum, ItemStack buyStack, boolean fillStacks)
    {
        Slot slot = gui.inventorySlots.getSlot(slotNum);

        // If there are items not matching the merchant recipe, move them out first
        if (areStacksEqual(buyStack, slot.getStack()) == false)
        {
            this.shiftClickSlot(gui.inventorySlots, gui.mc, slotNum);
        }

        this.moveItemsFromInventory(gui, slotNum, gui.mc.player.inventory, buyStack, fillStacks);
    }

    private void moveItemsFromInventory(GuiContainer gui, int slotTo, IInventory invSrc, ItemStack stackTemplate, boolean fillStacks)
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
                    if (this.clickSlotsToMoveItems(container, gui.mc, slot.slotNumber, slotTo) == false)
                    {
                        break;
                    }
                }
                else
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slot.slotNumber, slotTo);
                    break;
                }
            }
        }
    }

    private static boolean areStacksEqual(ItemStack stack1, ItemStack stack2)
    {
        return ItemStack.areItemsEqual(stack1, stack2) && ItemStack.areItemStackTagsEqual(stack1, stack2);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2)
    {
        if ((slot1 instanceof SlotItemHandler) && (slot2 instanceof SlotItemHandler))
        {
            return ((SlotItemHandler)slot1).getItemHandler() == ((SlotItemHandler)slot2).getItemHandler();
        }

        return slot1.inventory == slot2.inventory;
    }

    private ItemStack[] getOriginalStacks(Container container)
    {
        ItemStack[] originalStacks = new ItemStack[container.inventorySlots.size()];

        for (int i = 0; i < originalStacks.length; i++)
        {
            originalStacks[i] = container.inventorySlots.get(i).getStack().copy();
        }

        return originalStacks;
    }

    private void restoreOriginalStacks(Container container, ItemStack[] originalStacks)
    {
        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackSlot = container.getSlot(i).getStack();
            if (areStacksEqual(stackSlot, originalStacks[i]) == false ||
                (stackSlot.isEmpty() == false && originalStacks[i].isEmpty() == false &&
                stackSlot.getCount() != originalStacks[i].getCount()))
            {
                container.putStackInSlot(i, originalStacks[i]);
            }
        }
    }

    private int getTargetSlot(Container container, ItemStack[] originalStacks)
    {
        List<Slot> slots = container.inventorySlots;

        for (int i = 0; i < originalStacks.length; i++)
        {
            ItemStack stackOrig = originalStacks[i];
            ItemStack stackNew = slots.get(i).getStack();

            if ((stackOrig.isEmpty() && stackNew.isEmpty() == false) ||
               (stackOrig.isEmpty() == false && stackNew.isEmpty() == false && stackNew.getCount() == (stackOrig.getCount() + 1)))
            {
                return i;
            }
        }

        return -1;
    }

    private void shiftClickSlot(Container container, Minecraft mc, int slot)
    {
        mc.playerController.windowClick(container.windowId, slot, 0, ClickType.QUICK_MOVE, mc.player);
    }

    private boolean clickSlotsToMoveSingleItem(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        //System.out.println("clickSlotsToMoveSingleItem(from: " + slotFrom + ", to: " + slotTo + ")");

        ItemStack stack = container.inventorySlots.get(slotFrom).getStack();
        if (stack.isEmpty())
        {
            return false;
        }

        EntityPlayer player = mc.player;

        // Click on the from-slot to take items to the cursor - if there is more than one item in the from-slot,
        // right click on it, otherwise left click.
        mc.playerController.windowClick(container.windowId, slotFrom, stack.getCount() > 1 ? 1 : 0, ClickType.PICKUP, player);

        // Right click on the target slot to put one item to it
        mc.playerController.windowClick(container.windowId, slotTo, 1, ClickType.PICKUP, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack().isEmpty() == false)
        {
            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
        }

        return true;
    }

    private boolean clickSlotsToMoveSingleItemByShiftClick(Container container, Minecraft mc, int slotFrom)
    {
        EntityPlayer player = mc.player;
        ItemStack stack = container.inventorySlots.get(slotFrom).getStack();
        if (stack.isEmpty())
        {
            return false;
        }

        if (stack.getCount() > 1)
        {
            // Left click on the from-slot to take all the items to the cursor
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);

            // Still items left in the slot, put the stack back and abort
            if (container.inventorySlots.get(slotFrom).getHasStack())
            {
                mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
                return false;
            }
            else
            {
                // Right click one item back to the slot
                mc.playerController.windowClick(container.windowId, slotFrom, 1, ClickType.PICKUP, player);
            }
        }

        // ... and then shift-click on the slot
        this.shiftClickSlot(container, mc, slotFrom);

        if (player.inventory.getItemStack().isEmpty() == false)
        {
            // ... and then return the rest of the items
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
        }

        return true;
    }

    /**
     * Try move items from slotFrom to slotTo
     * @return true if at least some items were moved
     */
    private boolean clickSlotsToMoveItems(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        EntityPlayer player = mc.player;
        //System.out.println("clickSlotsToMoveItems(from: " + slotFrom + ", to: " + slotTo + ")");

        // Left click to take items
        mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);

        // Couldn't take the items, bail out now
        if (player.inventory.getItemStack().isEmpty())
        {
            return false;
        }

        boolean ret = true;
        int size = player.inventory.getItemStack().getCount();

        // Left click on the target slot to put the items to it
        mc.playerController.windowClick(container.windowId, slotTo, 0, ClickType.PICKUP, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack().isEmpty() == false)
        {
            ret = player.inventory.getItemStack().getCount() != size;

            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, ClickType.PICKUP, player);
        }

        return ret;
    }
}
