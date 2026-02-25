# Detect Misaligned Images — Flow Diagram

This document describes the flow when using the **Detect Misaligned Images** feature in Add Intensities (AISE/AIME).

## Overview

```mermaid
flowchart TB
    subgraph AddIntensitiesExp["AddIntensitiesExp Window"]
        Load["User selects folder<br/><small>browseFolder() → preprocessfolder() [AISE only]</small>"]
        Enable["Enable 'Detect Misaligned Images' button<br/><small>onNewFileSelected() → checkImagesButton.setEnabled(True)</small>"]
        Click["User clicks button<br/><small>checkImagesButton.clicked → checkImages()</small>"]
    end

    subgraph checkImages["checkImages()"]
        Dialog{"Create or reuse UnalignedImagesDialog<br/><small>unalignedImagesDialog.exec_()</small>"}
        Cancel["User cancels → exit"]
        LoadCache{"<small>loadImagesCache()</small><br/>cache exists?"}
        ModeCheck{"distance_mode != 1?"}
        Process["Compute center + angle per image<br/><small>getCenter(), getRotationAngle()</small>"]
        SkipCalc["Skip center/angle<br/><small>center=0, angle=0</small>"]
        UseCache["<small>searchInfoCache()</small>"]
        AddImg["<small>addImages(image, center, angle, filename)</small>"]
        Detect["<small>detectImages(dir_path, max_intensity_p95, distance_mode, coefficients)</small>"]
        SaveCacheCheck{"distance_mode != 1?"}
        SaveCache["<small>saveImagesCache(infoCache)</small>"]
    end

    subgraph Results["Results"]
        None["No misaligned images<br/><small>QMessageBox.information()</small>"]
        Found["Collect misaligned names<br/><small>image.getName() → misaligned_images</small>"]
        AISE{"Mode == 'aise'?"}
        ChoiceDialog["User chooses action<br/><small>QInputDialog.getItem()</small>"]
        Cancelled["User cancels → no action"]
        Review["<small>selectImageSequence()</small><br/>→ AISEImageSelectionWindow"]
        Ignore["Remove from groups<br/><small>onGroupChanged()</small>"]
        AIME_Show["Show list of misaligned<br/><small>QMessageBox.information()</small>"]
    end

    Load --> Enable
    Enable --> Click
    Click --> Dialog
    Dialog -->|Cancel| Cancel
    Dialog -->|OK| LoadCache
    LoadCache -->|No cache| ModeCheck
    LoadCache -->|Cache hit| UseCache
    UseCache --> AddImg
    ModeCheck -->|Yes| Process
    ModeCheck -->|No - Image mode| SkipCalc
    Process --> AddImg
    SkipCalc --> AddImg
    AddImg --> Detect
    Detect --> SaveCacheCheck
    SaveCacheCheck -->|Yes| SaveCache
    SaveCacheCheck -->|No - Image mode| None
    SaveCache --> None
    SaveCache --> Found
    None
    Found --> AISE
    AISE -->|Yes| ChoiceDialog
    ChoiceDialog -->|Cancel| Cancelled
    ChoiceDialog -->|Review selected images| Review
    ChoiceDialog -->|Ignore all misaligned| Ignore
    AISE -->|No - AIME| AIME_Show
```

## UnalignedImagesDialog — Detection Settings

```mermaid
flowchart LR
    subgraph Dialog["UnalignedImagesDialog"]
        DM["Distance Mode<br/><small>distanceMode.currentIndex() + 1</small>"]
        COEF["Coefficients<br/><small>imageCoefficient, centerCoefficient, angleCoefficient</small>"]
        OK["OK Button<br/><small>onExitPressed() → accept()</small>"]
    end

    subgraph Modes["Distance Modes"]
        M1["1: Image"]
        M2["2: Center"]
        M3["3: Center + Angle"]
        M4["4: Center + Image"]
        M5["5: Center + Angle + Image"]
    end

    subgraph Coefs["Coefficients (default 0.33 each)"]
        C1["Image"]
        C2["Center"]
        C3["Angle"]
    end

    DM --> Modes
    COEF --> Coefs
    OK --> checkImages
```

## AISE Image Selection (after "Review selected images")

```mermaid
flowchart TB
    subgraph AISEImageSelectionWindow["AISEImageSelectionWindow"]
        List["Show image list / thumbnails<br/><small>load_images()</small>"]
        Misaligned["Red border on misaligned images"]
        Select["Select Sequence button<br/><small>onSelectImageClicked()</small>"]
        Edit["Edit Sequence button<br/><small>editSequenceClicked()</small>"]
        Reset["Reset Sequence button<br/><small>resetSequenceClicked()</small>"]
        Confirm["Confirm button<br/><small>confirmedClicked() → accept()</small>"]
    end

    subgraph SelectFlow["Select Sequence flow"]
        First["Click first image<br/><small>checkboxChecked() → stores firstImage</small>"]
        Last["Click last image<br/><small>checkboxChecked() → builds range</small>"]
        AddGrp["Add range to img_grps<br/><small>clear_checkboxes()</small>"]
    end

    subgraph EditFlow["Edit Sequence flow"]
        EnableCB["1st click: enable checkboxes on grouped images<br/><small>load_checkboxes(True)</small>"]
        Toggle["User toggles checkboxes<br/><small>checkboxChecked() → img_to_delete</small>"]
        FinishEdit["2nd click: 'Finish Editing'<br/><small>editSequenceClicked()</small>"]
        Delete["<small>delete_images_from_grp()</small><br/><small>clear_checkboxes()</small>"]
    end

    List --> Misaligned
    Select --> First --> Last --> AddGrp
    Edit --> EnableCB --> Toggle --> FinishEdit --> Delete
    Confirm --> Return["Return img_grps to AddIntensitiesExp<br/><small>selectImageSequence() updates img_grps</small>"]
```

## Component Relationships

```mermaid
flowchart TB
    AddIntensitiesExp["AddIntensitiesExp<br/><small>browseFolder(), checkImages(), selectImageSequence(), onGroupChanged()</small>"]
    UnalignedImagesDialog["UnalignedImagesDialog<br/><small>onExitPressed()</small>"]
    AISEImageSelectionWindow["AISEImageSelectionWindow<br/><small>load_images(), checkboxChecked(), delete_images_from_grp()</small>"]
    XRayViewerGUI["XRayViewerGUI<br/><small>onNewFileSelected(), onImageChanged()</small>"]
    detectImages["detect_unaligned_images<br/><small>addImages(), detectImages()</small>"]

    AddIntensitiesExp -->|creates/reuses| UnalignedImagesDialog
    AddIntensitiesExp -->|calls| detectImages
    AddIntensitiesExp -->|creates/reuses| AISEImageSelectionWindow
    AISEImageSelectionWindow -->|"opens for preview via onLabelClicked()"| XRayViewerGUI
```
