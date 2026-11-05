package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, Path}

class DocsHeadingTest extends AnyFlatSpec, Matchers:
  private def headingLines(path: String): Set[String] =
    NFiles.readString(Path.of(path), StandardCharsets.UTF_8).linesIterator.toSet

  private def assertHeadings(path: String, required: Seq[String]): Unit =
    val present: Set[String] = headingLines(path)
    required.foreach: heading =>
      withClue(s"$path $heading "):
        present should contain(heading)

  "docs/commands.md" should "contain the operator headings" in:
    assertHeadings(
      "docs/commands.md",
      Seq(
        "### photo ls",
        "### photo repair",
        "### photo item show",
        "### photo item set",
        "### photo item month",
        "### photo item into",
        "### photo album ls",
        "### photo album show",
        "### photo album claims",
        "### photo album new",
        "### photo album rm",
        "### photo album mv",
        "### photo album include",
        "### photo album exclude",
        "### photo album merge",
        "### photo publish",
        "### photo sync",
        "### photo push",
        "### photo refresh",
        "### photo reconcile",
        "### photo file",
        "### photo card",
        "### photo card copy",
        "### photo login",
        "### photo status",
        "### photo logout",
        "### photo shell"
      )
    )

  "docs/web.md" should "contain the photo web heading" in:
    assertHeadings("docs/web.md", Seq("### photo web"))

  "docs/agent.md" should "contain the agent headings" in:
    assertHeadings(
      "docs/agent.md",
      Seq(
        "### photo agent stems",
        "### photo agent years",
        "### photo agent months",
        "### photo agent days",
        "### photo agent claims",
        "### photo agent catalog",
        "### photo agent items",
        "### photo agent match",
        "### photo agent locate",
        "### photo agent include",
        "### photo agent album-file",
        "### photo agent rename-album",
        "### photo agent set-description",
        "### photo agent set-date",
        "### photo agent download-item",
        "### photo agent add-to-album",
        "### photo agent remove-from-album",
        "### photo agent move-item",
        "### photo agent delete-item",
        "### photo agent upload-item",
        "### photo agent create-album",
        "### photo agent delete-album",
        "### photo agent drift",
        "### photo agent missing",
        "### photo serve",
        "### photo mcp"
      )
    )
