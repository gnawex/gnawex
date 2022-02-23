defmodule Docs.MixProject do
  use Mix.Project

  def project do
    [
      app: :docs,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Docs
      name: "GNAWEX",
      source_url: "https://github.com/gnawex/gnawex",
      homepage_url: "http://gnawex.com",
      docs: docs()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp docs do
    [
      main: "gnawex", 
      logo: "./logo.svg",
      extra_section: "GUIDES",
      extras: extras(),
      groups_for_extras: groups_for_extras(),
    ]
  end

  defp extras() do
    [
      "guides/gnawex.md",
      "guides/introduction/setup.md",
      "guides/introduction/available-scripts.md"
    ]
  end

  defp groups_for_extras do
    [
      Introduction: ~r/guides\/introduction\/.?/,
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.28", only: :dev, runtime: false},
    ]
  end
end
