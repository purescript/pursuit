## <a name="submitting-packages"></a>How to submit packages

Previously, users would need to use `bower` to upload packages' documentation. Uploading documentation is now handled by the PureScript Registry (still in alpha at the time of this writing). See https://discourse.purescript.org/t/registry-alpha-launched/3146 and any updates mentioned there for how to register and publish packages and their documentation.

## <a name="package-deprecation"></a>How to mark package as deprecated

Package deprecation is a mechanism to tell the end users that your package is no longer supported. When package is marked as deprecated, its contents will not show up in search results on Pursuit (with the only exception of the package name itself). A package can be marked as deprecated by adding a special keyword `pursuit-deprecated` to keywords section of `bower.json` and publishing a new version of the package.

## <a name="package-badges"></a>Package badges

Pursuit can generate SVG badges for your packages, which you can put on your project's homepage, or perhaps its GitHub readme.

The suggested markup for a badge is:

```
<a href="https://pursuit.purescript.org/packages/$PACKAGE_NAME">
  <img src="https://pursuit.purescript.org/packages/$PACKAGE_NAME/badge"
       alt="$PACKAGE_NAME on Pursuit">
  </img>
</a>
```
